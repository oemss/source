using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Diagnostics;
using System.Linq;
using System.Text;
using System.Windows.Forms;
using TypeLib;
using Library2;

using System.Threading;
using System.IO;
using System.Reflection;

public struct array_l
{
    public int x;
    public int y;
    public String way;
    public String text;
    public Boolean assum;
}
public struct array_code
{
    public String fst;
    public String snd;
}
public struct listassum
{
    public String way;
    public Boolean assum;
}
public struct st_find_el
{
    public Boolean fst;
    public int snd;
}
namespace WindowsFormsApplication2
{
    public partial class Form1 : Form
    {
        private static int all_value = 200;
        private Boolean check = false;
        private static Boolean press = false;
        private Boolean _check = true;
        private Boolean start_step = false;
        private Boolean rule = false;
        private Boolean _check_ar = false;
        private Boolean panel = false;
        int cord_x = 400;
        public int cord_y = 400;
        string text = "1";
        public Form1()
        {
            InitializeComponent();
            abc1();
        }
        private void Form1_Load(object sender, EventArgs e)
        {
            v_pan();
        }
        Label[] term = new Label[all_value];
        Label[] type = new Label[all_value];
        Label[] sd = new Label[all_value];
        Label[] eq = new Label[200];
        private void button1_Click(object sender, EventArgs e)
        {
            try
            {
                if (!checkBox1.Checked && !checkBox2.Checked)
                {
                    if (textBox1.Text.IndexOf('\\') != -1)
                    {
                        clear_label();
                        array_label(Term2.treelam(textBox1.Text), Term2.out_rule(textBox1.Text));
                        panel4.HorizontalScroll.Value = 800;
                        panel4.VerticalScroll.Value = 800;
                        if (!_check_ar)
                        {
                            panel2.HorizontalScroll.Value = 798;
                            panel2.VerticalScroll.Value = 798;
                        }
                        else
                        {
                            panel2.HorizontalScroll.Value = 0;
                            panel2.VerticalScroll.Value = 0;
                        }
                        textBox2.Text = "";
                        System.Diagnostics.Stopwatch sw = new Stopwatch();
                        sw.Start();
                        equat();
                        sw.Stop();
                        textBox2.Text += Environment.NewLine;
                        textBox2.Text += "Время выполнения: " + (sw.ElapsedMilliseconds / 100.0).ToString() + " секунд.";
                        start_step = false;
                        _i = 1;
                        _j = 0;
                    }
                    else
                    {
                        Form4 dialog = new Form4();
                        this.Visible = false;
                        _press();
                        dialog.label_change("Ошибка в записи терма");
                        Point point = new Point(Location.X + 777 / 2 - 50, Location.Y + 573 / 2 - 20);
                        dialog.Location = point;
                        dialog.Show();
                    }
                }
                else
                {
                    if(checkBox1.Checked)
                    {
                        clear_label();
                        
                        textBox2.Text = "Типа выражения системы лямбда w с подчёркиванием: " + Term2.deftype_lw(textBox1.Text);
                        start_step = false;
                        _i = 1;
                        _j = 0;
                    }
                    else
                    {
                        clear_label();
                        textBox2.Text = "Типа выражения системы лямбда p: " + Term2.deftype_lp(textBox1.Text);
                        start_step = false;
                        _i = 1;
                        _j = 0;
                    }
                }
            }
            catch
            {
                Form4 dialog = new Form4();
                this.Visible = false;
                _press();
                dialog.label_change("Ошибка в записи терма");
                Point point = new Point(Location.X + 777 / 2 - 50, Location.Y + 573 / 2 - 20);
                dialog.Location = point;
                dialog.Show();
            }
        }
        public void _vis()
        {
            this.Visible = true;
        }
        public void _press()
        {
            press = true;
        }
        public void _press_off()
        {
            press = false;
        }
        private void clear_label()
        {
            for (int i = 0; i < all_value; i++)
            {
                panel4.Controls.Remove(term[i]);
                panel3.Controls.Remove(sd[i]);
                panel2.Controls.Remove(type[i]);
            }
            array_t.Clear();
            //textBox1.Clear();
            textBox2.Clear();
            panel1.BackgroundImage = null;
        }
        private void move_label(int t, int x, int j)
        {
            if (t == 0)
                for (int i = 0; i < j; i++)
                {
                    /*Console.Write("i = ");
                    Console.WriteLine(i);
                    Console.WriteLine(term[i].Location.X);*/
                    term[i].Location = new Point(term[i].Location.X + x, term[i].Location.Y);
                }
            else
                for (int i = 0; i < j; i++)
                    type[i].Location = new Point(type[i].Location.X + x, type[i].Location.Y);
        }
        private void equat()
        {
            int i;
            List<array_code> arr_c = new List<array_code>();
            var cd = new array_code();
            String[,] s = tree(Term2.out_equat(textBox1.Text));
            for (i = 0; s[0, i] != "!"; i++)
            {
                textBox2.Text += s[0, i] + " = " + s[1, i] + Environment.NewLine;
            }
            textBox2.Text += "Тип терма: " + Environment.NewLine + Term2.deftype(textBox1.Text);
            textBox2.Font = new Font("Cambria", 12, textBox2.Font.Style);
        }
        List<array_l> array_t = new List<array_l>();
        private void array_label(String s, String s2)
        {
            List<array_l> array = new List<array_l>();
            List<String> assum = new List<String>();
            List<String> assu = new List<String>();
            List<array_code> arr_c = new List<array_code>();
            List<array_code> arr_t = new List<array_code>();
            var cd = new array_code();
            var tp = new array_code();
            var n = new array_l();
            var typ = new array_l();
            String[,] p = tree(s);
            String[,] t = sort(tree(s2));
            int i, j,
                shift_y = 25,
                shift_x = 15,
                ind;
            n.way = "0";
            typ.way = "0";
            n.x = 800;
            typ.x = 800;
            n.y = 800;
            typ.y = 800;
            n.text = p[1, 0];
            typ.text = t[1, 0];
            array.Add(n);
            array_t.Add(typ);
            Label widthlabel = new Label();
            widthlabel.Font = new Font(widthlabel.Font.Name, 12, widthlabel.Font.Style);
            for (i = 0; p[0, i] != "!"; i++)
            {
                if (Term2.sslast(p[0, i]) == "d")
                {
                    p[0, i] = Term2.sinit2(p[0, i]);
                    assum.Add(p[0, i]);
                }
                else
                {
                    if (Term2.sslast(p[0, i]) == "s")
                    {
                        assu.Add(p[1, i]);
                    }
                }
            }
            p = sort(p);
            for (i = 1; p[0, i] != "!"; i++)
            {
                cd.fst = p[0, i];
                tp.fst = t[0, i];
                cd.snd = p[1, i];
                tp.snd = t[1, i];
                arr_c.Add(cd);
                arr_t.Add(tp);
            }
            for (i = 0; arr_c.Count != 0;)
            {
                if (_last(arr_c[i].fst) != "s")
                {
                    if (_last(arr_c[i].fst) == "1")
                    {
                        if ((ind = _find(_init(arr_c[i].fst), arr_c, 1)) != -1)
                        {
                            n.way = arr_c[i].fst;
                            n.text = arr_c[i].snd;
                            widthlabel.Text = n.text;
                            foreach (array_l ar in array)
                            {
                                if (_init(n.way) == ar.way)
                                {
                                    if (check_0(n.way))
                                        n.x = ar.x;
                                    else
                                        n.x = ar.x - shift_x;
                                    n.y = ar.y - shift_y;
                                    break;
                                }
                            }
                            array.Add(n);
                            n.way = arr_c[ind].fst;
                            n.text = arr_c[ind].snd;
                            n.x += widthlabel.PreferredWidth + shift_x;
                            array.Add(n);
                            arr_c.RemoveAt(ind);
                            arr_c.RemoveAt(i);
                        }
                        else
                        {
                            n.way = arr_c[i].fst;
                            n.text = arr_c[i].snd;
                            widthlabel.Text = n.text;
                            foreach (array_l ar in array)
                            {
                                if (_init(n.way) == ar.way)
                                {
                                    n.x = ar.x;
                                    n.y = ar.y - shift_y;
                                    break;
                                }
                            }
                            array.Add(n);
                            arr_c.RemoveAt(i);
                        }
                    }
                    else
                    {
                        if ((ind = _find(_init(arr_c[i].fst), arr_c, 0)) != -1)
                        {
                            n.way = arr_c[ind].fst;
                            n.text = arr_c[ind].snd;
                            widthlabel.Text = n.text;
                            foreach (array_l ar in array)
                            {
                                if (_init(n.way) == ar.way)
                                {
                                    if (check_0(n.way))
                                        n.x = ar.x;
                                    else
                                        n.x = ar.x - shift_x;
                                    n.y = ar.y - shift_y;
                                    break;
                                }
                            }
                            array.Add(n);
                            n.way = arr_c[i].fst;
                            n.text = arr_c[i].snd;
                            n.x += widthlabel.PreferredWidth + shift_x;
                            array.Add(n);
                            arr_c.RemoveAt(ind);
                            arr_c.RemoveAt(i);
                        }
                        else
                        {
                            n.way = arr_c[i].fst;
                            n.text = arr_c[i].snd;
                            widthlabel.Text = n.text;
                            foreach (array_l ar in array)
                            {
                                if (_init(n.way) == ar.way)
                                {
                                    n.x = ar.x;
                                    n.y = ar.y - shift_y;
                                    break;
                                }
                            }
                            array.Add(n);
                            arr_c.RemoveAt(i);
                        }
                    }
                }
                else
                {
                    n.text = "Test";
                    n.way = "Test";
                    array.Add(n);
                    arr_c.RemoveAt(i);
                }
            }

            for (i = 0; arr_t.Count != 0;)
            {
                if (_last(arr_t[i].fst) != "s")
                {
                    if (_last(arr_t[i].fst) == "1")
                    {
                        if ((ind = _find(_init(arr_t[i].fst), arr_t, 1)) != -1)
                        {
                            n.way = arr_t[i].fst;
                            n.text = arr_t[i].snd;
                            widthlabel.Text = n.text;
                            foreach (array_l ar in array_t)
                            {
                                if (_init(n.way) == ar.way)
                                {
                                    n.y = ar.y - shift_y;
                                    n.x = ar.x;
                                    break;
                                }
                            }
                            array_t.Add(n);
                            n.way = arr_t[ind].fst;
                            n.text = arr_t[ind].snd;
                            n.x += widthlabel.PreferredWidth + shift_x;
                            array_t.Add(n);
                            arr_t.RemoveAt(ind);
                            arr_t.RemoveAt(i);
                        }
                        else
                        {
                            n.way = arr_t[i].fst;
                            n.text = arr_t[i].snd;
                            widthlabel.Text = n.text;
                            foreach (array_l ar in array_t)
                            {
                                if (_init(n.way) == ar.way)
                                {
                                    n.x = ar.x;
                                    n.y = ar.y - shift_y;
                                    break;
                                }
                            }
                            array_t.Add(n);
                            arr_t.RemoveAt(i);
                        }
                    }
                    else
                    {
                        if ((ind = _find(_init(arr_t[i].fst), arr_t, 0)) != -1)
                        {
                            n.way = arr_t[ind].fst;
                            n.text = arr_t[ind].snd;
                            widthlabel.Text = n.text;
                            foreach (array_l ar in array_t)
                            {
                                if (_init(n.way) == ar.way)
                                {
                                    n.x = ar.x;
                                    n.y = ar.y - shift_y;
                                    break;
                                }
                            }
                            array_t.Add(n);
                            n.way = arr_t[i].fst;
                            n.text = arr_t[i].snd;
                            n.x += widthlabel.PreferredWidth + shift_x;
                            array_t.Add(n);
                            arr_t.RemoveAt(ind);
                            arr_t.RemoveAt(i);
                        }
                        else
                        {
                            n.way = arr_t[i].fst;
                            n.text = arr_t[i].snd;
                            widthlabel.Text = n.text;
                            foreach (array_l ar in array_t)
                            {
                                if (_init(n.way) == ar.way)
                                {
                                    n.x = ar.x;
                                    n.y = ar.y - shift_y;
                                    if (n.way.Length >= 2)
                                        break;
                                }
                            }
                            array_t.Add(n);
                            arr_t.RemoveAt(i);
                        }
                    }
                }
            }
            term[0] = new Label();
            foreach (array_l ar in array)
            {
                if (ar.way == "0")
                {
                    term[0].Text = ar.text + "       ";
                    term[0].Location = new Point(ar.x, ar.y);
                    term[0].Font = new Font("Cambria", 12, term[0].Font.Style);
                    panel4.Controls.Add(term[0]);
                    term[0].AutoSize = true;
                    break;
                }
            }
            foreach (array_l ar in array_t)
            {
                Console.WriteLine(ar.way);
                Console.WriteLine(ar.text);
            }
            if (check_ar(array_t))
            {
                array_t = edit_tree(array_t);
                _check_ar = true;
            }
            else
            {
                _check_ar = false;
            }
            for (j = 1; j < array.Count(); j++)
            {
                for (; array[j].way == "Test"; j++) ;
                term[j] = new Label();
                term[j].Text = array[j].text;
                term[j].Location = new Point(array[j].x, array[j].y);
                term[j].Font = new Font("Cambria", 12, term[j].Font.Style);
                term[j].BackColor = System.Drawing.Color.Transparent;
                panel4.Controls.Add(term[j]);
                if (find_assum(array[j].way, array[j].text, assu, assum))
                {
                    term[j].ForeColor = System.Drawing.Color.Green;
                }
                term[j].AutoSize = true;
            }
            type[0] = new Label();
            foreach (array_l ar in array_t)
            {
                if (ar.way == "0")
                {
                    type[0].Text = ar.text;
                    type[0].Location = new Point(ar.x, ar.y);
                    type[0].Font = new Font("Cambria", 12, type[0].Font.Style);
                    panel2.Controls.Add(type[0]);
                    type[0].AutoSize = true;
                    break;
                }
            }
            for (j = 1; j < array_t.Count(); j++)
            {
                for (; array_t[j].way == "Test"; j++) ;
                type[j] = new Label();
                type[j].Text = array_t[j].text;
                type[j].Location = new Point(array_t[j].x, array_t[j].y);
                type[j].Font = new Font("Cambria", 12, type[j].Font.Style);
                type[j].BackColor = System.Drawing.Color.Transparent;
                panel2.Controls.Add(type[j]);
                type[j].AutoSize = true;
            }
            uniq = dist(assu);
            sd[0] = new Label();
            sd[0].Text = "Cписок допущений: ";
            sd_x = 0;
            sd_y = 0;
            sd[0].Location = new Point(sd_x, sd_y);
            sd[0].Font = new Font("Cambria", 12, sd[0].Font.Style);
            sd[0].AutoSize = true;
            panel3.Controls.Add(sd[0]);
            sd_y += 15;
            for (j = 0; j < uniq.Count(); j++)
            {
                sd[j + 1] = new Label();
                sd[j + 1].Text = (j + 1).ToString() + "." + uniq[j];
                sd[j + 1].Location = new Point(sd_x, sd_y);
                sd[j + 1].Font = new Font("Cambria", 12, sd[0].Font.Style);
                sd[j + 1].AutoSize = true;
                panel3.Controls.Add(sd[j + 1]);
                sd_y += 15;
            }
        }
        private List<String> dist(List<String> s)
        {
            List<String> s1 = new List<String>();
            for (int j = 0; j < s.Count; j++)
            {
                if (s1.IndexOf(s[j]) == -1)
                {
                    s1.Add(s[j]);
                }
            }
            return s1;
        }
        private int min_y(List<array_l> s)
        {
            int min = 1000;
            for (int j = 0; j < s.Count; j++)
            {
                if (s[j].y < min)
                    min = s[j].y;
            }
            return min;
        }
        private int max_x(List<array_l> s)
        {
            int max = -1;
            for (int j = 0; j < s.Count; j++)
            {
                if (s[j].x > max)
                    max = s[j].x;
            }
            return max;
        }
        private List<array_l> array_label2(String s, String s2)
        {
            List<array_l> array = new List<array_l>();
            List<String> assum = new List<String>();
            List<String> assu = new List<String>();
            List<array_code> arr_c = new List<array_code>();
            List<array_code> arr_t = new List<array_code>();
            var cd = new array_code();
            String[,] t = sort(tree(s2));
            var tp = new array_code();
            var n = new array_l();
            var typ = new array_l();
            String[,] p = tree(s);
            arr_tree = sort(tree(s));
            int i, j,
                shift_y = 25,
                shift_x = 15,
                ind;
            n.way = "0";
            typ.way = "0";
            n.x = 800;
            typ.x = 800;
            n.y = 800;
            typ.y = 800;
            n.text = p[1, 0];
            typ.text = t[1, 0];
            array.Add(n);
            array_t.Add(typ);
            Label widthlabel = new Label();

            widthlabel.Font = new Font(widthlabel.Font.Name, 12, widthlabel.Font.Style);
            for (i = 0; p[0, i] != "!"; i++)
            {
                if (Term2.sslast(p[0, i]) == "d")
                {
                    p[0, i] = Term2.sinit2(p[0, i]);
                    assum.Add(p[0, i]);
                }
                else
                {
                    if (Term2.sslast(p[0, i]) == "s")
                    {
                        assu.Add(p[1, i]);
                    }
                }
            }
            p = sort(p);
            for (i = 1; p[0, i] != "!"; i++)
            {
                cd.fst = p[0, i];
                tp.fst = t[0, i];
                cd.snd = p[1, i];
                tp.snd = t[1, i];
                arr_c.Add(cd);
                arr_t.Add(tp);
            }
            for (i = 0; arr_c.Count != 0;)
            {
                if (_last(arr_c[i].fst) != "s")
                {
                    if (_last(arr_c[i].fst) == "1")
                    {
                        if ((ind = _find(_init(arr_c[i].fst), arr_c, 1)) != -1)
                        {
                            n.way = arr_c[i].fst;
                            n.text = arr_c[i].snd;
                            widthlabel.Text = n.text;
                            foreach (array_l ar in array)
                            {
                                if (_init(n.way) == ar.way)
                                {
                                    if (check_0(n.way))
                                        n.x = ar.x;
                                    else
                                        n.x = ar.x - shift_x;
                                    n.y = ar.y - shift_y;
                                    break;
                                }
                            }
                            array.Add(n);
                            n.way = arr_c[ind].fst;
                            n.text = arr_c[ind].snd;
                            n.x += widthlabel.PreferredWidth + shift_x;
                            array.Add(n);
                            arr_c.RemoveAt(ind);
                            arr_c.RemoveAt(i);
                        }
                        else
                        {
                            n.way = arr_c[i].fst;
                            n.text = arr_c[i].snd;
                            widthlabel.Text = n.text;
                            foreach (array_l ar in array)
                            {
                                if (_init(n.way) == ar.way)
                                {
                                    n.x = ar.x;
                                    n.y = ar.y - shift_y;
                                    break;
                                }
                            }
                            array.Add(n);
                            arr_c.RemoveAt(i);
                        }
                    }
                    else
                    {
                        if ((ind = _find(_init(arr_c[i].fst), arr_c, 0)) != -1)
                        {
                            n.way = arr_c[ind].fst;
                            n.text = arr_c[ind].snd;
                            widthlabel.Text = n.text;
                            foreach (array_l ar in array)
                            {
                                if (_init(n.way) == ar.way)
                                {
                                    if (check_0(n.way))
                                        n.x = ar.x;
                                    else
                                        n.x = ar.x - shift_x;
                                    n.y = ar.y - shift_y;
                                    break;
                                }
                            }
                            array.Add(n);
                            n.way = arr_c[i].fst;
                            n.text = arr_c[i].snd;
                            n.x += widthlabel.PreferredWidth + shift_x;
                            array.Add(n);
                            arr_c.RemoveAt(ind);
                            arr_c.RemoveAt(i);
                        }
                        else
                        {
                            n.way = arr_c[i].fst;
                            n.text = arr_c[i].snd;
                            widthlabel.Text = n.text;
                            foreach (array_l ar in array)
                            {
                                if (_init(n.way) == ar.way)
                                {
                                    n.x = ar.x;
                                    n.y = ar.y - shift_y;
                                    break;
                                }
                            }
                            array.Add(n);
                            arr_c.RemoveAt(i);
                        }
                    }
                }
                else
                {
                    n.text = "Test";
                    n.way = arr_c[i].fst;
                    array.Add(n);
                    arr_c.RemoveAt(i);
                }
            }
            for (i = 0; arr_t.Count != 0;)
            {
                if (_last(arr_t[i].fst) != "s")
                {
                    if (_last(arr_t[i].fst) == "1")
                    {
                        if ((ind = _find(_init(arr_t[i].fst), arr_t, 1)) != -1)
                        {
                            n.way = arr_t[i].fst;
                            n.text = arr_t[i].snd;
                            widthlabel.Text = n.text;
                            foreach (array_l ar in array_t)
                            {
                                if (_init(n.way) == ar.way)
                                {
                                    n.y = ar.y - shift_y;
                                    n.x = ar.x;
                                    break;
                                }
                            }
                            array_t.Add(n);
                            n.way = arr_t[ind].fst;
                            n.text = arr_t[ind].snd;
                            n.x += widthlabel.PreferredWidth + shift_x;
                            array_t.Add(n);
                            arr_t.RemoveAt(ind);
                            arr_t.RemoveAt(i);
                        }
                        else
                        {
                            n.way = arr_t[i].fst;
                            n.text = arr_t[i].snd;
                            widthlabel.Text = n.text;
                            foreach (array_l ar in array_t)
                            {
                                if (_init(n.way) == ar.way)
                                {
                                    n.x = ar.x;
                                    n.y = ar.y - shift_y;
                                    break;
                                }
                            }
                            array_t.Add(n);
                            arr_t.RemoveAt(i);
                        }
                    }
                    else
                    {
                        if ((ind = _find(_init(arr_t[i].fst), arr_t, 0)) != -1)
                        {
                            n.way = arr_t[ind].fst;
                            n.text = arr_t[ind].snd;
                            widthlabel.Text = n.text;
                            foreach (array_l ar in array_t)
                            {
                                if (_init(n.way) == ar.way)
                                {
                                    n.y = ar.y - shift_y;
                                    n.x = ar.x;
                                    break;
                                }
                            }
                            array_t.Add(n);
                            n.way = arr_t[i].fst;
                            n.text = arr_t[i].snd;
                            n.x += widthlabel.PreferredWidth + shift_x;
                            array_t.Add(n);
                            arr_t.RemoveAt(ind);
                            arr_t.RemoveAt(i);
                        }
                        else
                        {
                            n.way = arr_t[i].fst;
                            n.text = arr_t[i].snd;
                            widthlabel.Text = n.text;
                            foreach (array_l ar in array_t)
                            {
                                if (_init(n.way) == ar.way)
                                {
                                    n.y = ar.y - shift_y;
                                    n.x = ar.x;
                                    break;
                                }
                            }
                            array_t.Add(n);
                            arr_t.RemoveAt(i);
                        }
                    }
                }
            }
            check = true;
            if (check_ar(array_t))
            {
                array_t = edit_tree(array_t);
                Console.WriteLine("/////////////////////////////////////2");
                foreach (array_l l in array_t)
                {
                    Console.WriteLine(l.text);
                    Console.WriteLine(l.way);
                }
                Console.WriteLine("/////////////////////////////////////2");
                array_t = sort_ar(array_t, array);
                Console.WriteLine("/////////////////////////////////////");
                foreach (array_l l in array)
                {
                    Console.WriteLine(l.text);
                    Console.WriteLine(l.way);
                }
                Console.WriteLine("/////////////////////////////////////");
                check = false;
            }



            return array;
        }
        private int _find(String s, List<array_code> t, int k)
        {
            int i;
            if (k == 1)
            {
                for (i = 0; i < t.Count; i++)
                    if (_last(t[i].fst) == "d")
                    {
                        if (_init(t[i].fst) == s + "0")
                            return i;
                    }
                    else
                    {
                        if (t[i].fst == s + "0")
                        {
                            return i;
                        }
                    }
            }
            else
            {
                for (i = 0; i < t.Count; i++)
                    if (_last(t[i].fst) == "d")
                    {
                        if (_init(t[i].fst) == s + "1")
                            return i;
                    }
                    else
                    {
                        if (t[i].fst == s + "1")
                        {
                            return i;
                        }
                    }
            }
            return -1;
        }
        private String _init(String s)
        {
            return Term2.sinit2(s);
        }
        private String _last(String s)
        {
            return Term2.sslast(s);
        }
        private List<String> __assum(String[,] p)
        {
            List<String> _assum = new List<String>();
            int i;
            for (i = 0; p[0, i] != "!"; i++)
                if (Term2.sslast(p[0, i]) == "d")
                    _assum.Add(Term2.sinit2(p[0, i]));
            return _assum;
        }
        private List<String> __assu(String[,] p)
        {
            List<String> _assu = new List<String>();
            int i;
            for (i = 0; p[0, i] != "!"; i++)
                if (Term2.sslast(p[0, i]) == "s")
                    _assu.Add(p[1, i]);
            return _assu;
        }
        String[,] arr_tree;
        static Boolean find_assum(String way, String text, List<String> s, List<String> t)
        {
            foreach (String ar in t)
                if (ar == way)
                    foreach (String ar2 in s)
                        if (ar2 == text)
                            return true;
            return false;
        }
        static String[,] tree(String s)
        {
            String[,] p = new String[2, all_value];
            int pos = 0, start;
            int flag = 0;
            int i;
            for (i = 0; s[pos] != '!' && s[pos] != ']';)
            {
                if (s[pos] == '[')
                {
                    if (flag == 1)
                        flag = 0;
                    pos += 1;
                }
                else
                {
                    if (s[pos] == ',')
                    {
                        flag = 1;
                        pos += 1;
                    }
                    else
                    {
                        for (start = pos; s[pos] != ']' && s[pos] != ','; pos++)
                        {

                        }
                        if (flag == 0)
                        {
                            p[0, i] = s.Substring(start, pos - start);

                        }
                        else
                        {

                            p[1, i] = s.Substring(start, pos - start);
                            i += 1;
                            pos += 1;

                        }
                    }
                }
            }
            p[0, i] = "!";
            return p;
        }
        static String[,] sort(String[,] s)
        {
            int i, j;
            String swap1, swap2;
            for (i = 1; s[0, i] != "!"; i++)
            {
                for (j = i + 1; s[0, j] != "!"; j++)
                {
                    if (s[0, i].Length > s[0, j].Length)
                    {
                        swap1 = s[0, i];
                        swap2 = s[1, i];
                        s[0, i] = s[0, j];
                        s[1, i] = s[1, j];
                        s[0, j] = swap1;
                        s[1, j] = swap2;
                    }
                }
            }
            return s;
        }
        private void Delete_Click(object sender, EventArgs e)
        {
            try
            {

                clear_label();
                _i = 1;
                _j = 0;
                start_step = false;
            }
            catch
            {

            }
        }
        int _i = 1;
        int _j = 0;
        int sd_x;
        int sd_y;
        List<array_l> steparr = new List<array_l>();
        List<String> uniq = new List<String>();
        List<String> ___assum = new List<String>();
        List<String> ___assu = new List<String>();
        private void button2_Click(object sender, EventArgs e)
        {

            panel2.HorizontalScroll.Value = 0;
            panel2.VerticalScroll.Value = 0;
            panel4.HorizontalScroll.Value = 0;
            panel4.VerticalScroll.Value = 0;
            try
            {
                if (!start_step)
                {
                    clear_label();
                    steparr = array_label2(Term2.treelam(textBox1.Text), Term2.out_rule(textBox1.Text));
                    ___assu = __assu(arr_tree);
                    ___assum = __assum(arr_tree);
                    uniq = dist(___assu);
                    sd[0] = new Label();
                    sd[0].Text = "Cписок допущений: ";
                    sd_y = 0;
                    sd[0].Font = new Font("Cambria", 12, sd[0].Font.Style);
                    sd[0].AutoSize = true;
                    term[0] = new Label();
                    term[0].Text = steparr[0].text;
                    sd_x = 0;
                    sd[0].Location = new Point(sd_x, sd_y);
                    panel3.Controls.Add(sd[0]);
                    sd_y += 15;
                    term[0].Location = new Point(steparr[0].x, steparr[0].y);
                    term[0].Font = new Font("Cambria", 12, term[0].Font.Style);
                    panel4.Controls.Add(term[0]);
                    type[0] = new Label();
                    foreach (array_l ar in array_t)
                    {
                        if (ar.way == "0")
                        {
                            type[0].Text = ar.text;
                            type[0].Location = new Point(ar.x, ar.y);
                            type[0].Font = new Font("Cambria", 12, type[0].Font.Style);
                            panel2.Controls.Add(type[0]);
                            type[0].AutoSize = true;
                            break;
                        }
                    }

                    term[0].AutoSize = true;
                    start_step = true;
                }
                else
                {
                    if (_i < steparr.Count)
                    {
                        for (; steparr[_i].text == "Test" && _j < uniq.Count;)
                        {
                            sd[_j + 1] = new Label();
                            sd[_j + 1].Text = (_j + 1).ToString() + "." + uniq[_j];
                            sd[_j + 1].Location = new Point(sd_x, sd_y);
                            sd[_j + 1].Font = new Font("Cambria", 12, sd[0].Font.Style);
                            sd[_j + 1].AutoSize = true;
                            panel3.Controls.Add(sd[_j + 1]);
                            sd_y += 15;
                            type[_i] = new Label();
                            type[_i].Text = array_t[_i].text;
                            type[_i].Location = new Point(array_t[_i].x, array_t[_i].y);
                            type[_i].Font = new Font("Cambria", 12, type[_i].Font.Style);
                            type[_i].BackColor = System.Drawing.Color.Transparent;
                            panel2.Controls.Add(type[_i]);
                            //panel2.Controls.Add(type[_i]);
                            type[_i].AutoSize = true;
                            _j++;
                            _i++;
                        }
                        //for (; steparr[_i].way == "Test";) ;
                        for (int h = 0; h < steparr.Count; h++)
                        {
                            if (Term2.sslast(steparr[_i].way) == "s")
                            {
                                panel1.BackgroundImage = System.Drawing.Bitmap.FromFile("image\\3.png");
                                break;
                            }
                            if (Term2.sslast(steparr[_i].way) == "1")
                            {
                                if (Term2.sinit2(steparr[_i].way) + "0" == steparr[h].way)
                                {
                                    if (steparr[_i].text != "Test")
                                    {
                                        panel1.BackgroundImage = System.Drawing.Bitmap.FromFile("image\\2.png");
                                        break;
                                    }
                                    else
                                    {
                                        panel1.BackgroundImage = System.Drawing.Bitmap.FromFile("image\\3.png");
                                        break;
                                    }
                                }
                            }
                            else
                            {
                                if (Term2.sslast(steparr[_i].way) == "0")
                                {
                                    if (Term2.sinit2(steparr[_i].way) + "1" == steparr[h].way)
                                    {
                                        if (steparr[_i].text != "Test")
                                        {
                                            panel1.BackgroundImage = System.Drawing.Bitmap.FromFile("image\\2.png");
                                            break;
                                        }
                                        else
                                        {
                                            panel1.BackgroundImage = System.Drawing.Bitmap.FromFile("image\\3.png");
                                            break;
                                        }
                                    }
                                }
                                else
                                {
                                    panel1.BackgroundImage = System.Drawing.Bitmap.FromFile("image\\3.png");
                                }
                            }
                            panel1.BackgroundImage = System.Drawing.Bitmap.FromFile("image\\3.png");
                        }
                        term[_i] = new Label();
                        term[_i].Text = steparr[_i].text;
                        /*Console.WriteLine("--------------");
                        Console.WriteLine(steparr[_i].x);
                        Console.WriteLine(steparr[_i].y);
                        Console.WriteLine(steparr[_i].way);
                        Console.WriteLine("--------------");*/
                        term[_i].Location = new Point(steparr[_i].x, steparr[_i].y);
                        term[_i].Font = new Font("Cambria", 12, term[_i].Font.Style);
                        term[_i].BackColor = System.Drawing.Color.Transparent;
                        panel4.Controls.Add(term[_i]);
                        if (find_assum(steparr[_i].way, steparr[_i].text, ___assu, ___assum))
                        {
                            term[_i].ForeColor = System.Drawing.Color.Green;
                        }

                        //for (; array_t[_i].way == "Test"; _i++) ;
                        type[_i] = new Label();
                        type[_i].Text = array_t[_i].text;

                        type[_i].Location = new Point(array_t[_i].x, array_t[_i].y);
                        type[_i].Font = new Font("Cambria", 12, type[_i].Font.Style);
                        type[_i].BackColor = System.Drawing.Color.Transparent;
                        panel2.Controls.Add(type[_i]);
                        type[_i].AutoSize = true;

                        term[_i].AutoSize = true;
                        _i++;
                    }
                }
                panel4.HorizontalScroll.Value = 800;
                panel4.VerticalScroll.Value = 800;
                if (check)
                {
                    panel2.HorizontalScroll.Value = 800;
                    panel2.VerticalScroll.Value = 800;
                }
                //panel2.HorizontalScroll.Value = 800;
                //panel2.VerticalScroll.Value = 800;
            }
            catch
            {
                Form4 dialog = new Form4();
                this.Visible = false;
                dialog.label_change("Ошибка в записи терма");
                Point point = new Point(Location.X + 777 / 2 - 50, Location.Y + 573 / 2 - 20);
                dialog.Location = point;
                dialog.Show();
            }
        }
        private void button3_Click(object sender, EventArgs e)
        {
            try
            {
                String path = Directory.GetCurrentDirectory() + "\\Terms.txt";
                String line, line2 = "1.";
                int i;
                Term2.deftype(textBox1.Text);
                if (File.Exists(path))
                {
                    StreamReader fs = new StreamReader(@path);
                    while ((line = fs.ReadLine()) != null)
                    {
                        if (line.IndexOf('.') != -1)
                        {
                            line2 = line;
                        }
                    }
                    for (i = 0; line2[i] != '.'; i++) ;
                    line = line2.Substring(0, i);
                    i = 1 + Convert.ToInt32(line, 10);
                    fs.Close();
                    using (var writer = new StreamWriter(@path, true))
                    {
                        writer.Write("\n" + i.ToString() + "." + textBox1.Text);
                    }
                    //File.AppendAllText(path, "\n" + i.ToString() + "." + textBox1.Text, Encoding.UTF8);
                }
                else
                {
                    using (FileStream fs = File.Create(path))
                    {
                        Byte[] info = new UTF8Encoding(true).GetBytes("1." + textBox1.Text);
                        fs.Write(info, 0, info.Length);
                        fs.Close();
                    }
                }
            }
            catch
            {
                Form4 dialog = new Form4();
                this.Visible = false;
                dialog.label_change("Ошибка с файлом или ошибка в записи терма");
                Point point = new Point(Location.X + 777 / 2 - 50, Location.Y + 573 / 2 - 20);
                dialog.Location = point;
                dialog.Show();
            }
        }
        private void button4_Click(object sender, EventArgs e)
        {
            try
            {
                //Console.WriteLine(Term.out_rule(textBox1.Text));
                textBox2.Text = "";
                System.Diagnostics.Stopwatch sw = new Stopwatch();
                sw.Start();
                equat();
                sw.Stop();
                textBox2.Text += "\n \n Время выполнения: " + (sw.ElapsedMilliseconds / 100.0).ToString() + " секунд.";
            }
            catch
            {
                Form4 dialog = new Form4();
                this.Visible = false;
                _press();
                dialog.label_change("Ошибка в выводе уравнений");
                Point point = new Point(Location.X + 777 / 2 - 50, Location.Y + 573 / 2 - 20);
                dialog.Location = point;
                dialog.Show();
            }
        }
        private void v_pan()
        {
            if (!panel)
            {
                Controls.Add(panel4);
                panel4.AutoScroll = true;
                Controls.Add(panel2);
                panel2.AutoScroll = true;
                Controls.Add(panel3);
                panel3.AutoScroll = true;
                Controls.Add(panel1);
                panel = true;
            }
        }
        private void panel1_Paint(object sender, PaintEventArgs e)
        {

        }
        private Boolean check_0(String s)
        {
            for (int i = 0, l = s.Length; i < l - 1; i++)
                if (s[i] != '0')
                    return false;
            return true;
        }
        public void random_term()
        {
            try
            {
                String line;
                int i = 1;
                String path = Directory.GetCurrentDirectory() + "\\Terms.txt";
                if (File.Exists(path))
                {
                    StreamReader fs = new StreamReader(@path);
                    for (;;)
                    {
                        if ((line = fs.ReadLine()) != null && line.IndexOf(".") != -1)
                        {
                            i++;
                        }
                    }

                    fs.Close();
                }
            }
            catch
            {

            }
        }
        public void tree_view(List<array_l> arr)
        {
            String way = "0", way_s = "0";
            List<array_l> arr_tree = new List<array_l>();
            array_l n;
            n = arr[0];
            arr_tree.Add(n);
            for (; arr.Count != 0;)
            {

            }
        }
        public List<array_l> edit_tree(List<array_l> ar2)
        {
            List<array_l> ar = new List<array_l>();
            ar = ar2;
            List<array_l> array = new List<array_l>();
            array_l n;
            st_find_el st_find;
            int i, k,
                _shift_y = 15,
               _shift_x = 15,
                max_length;
            //max_length = ar[ar.Count].way.Length;
            String way = "0";
            Label widthlabel = new Label();
            array.Clear();
            widthlabel.Font = new Font(widthlabel.Font.Name, 12, widthlabel.Font.Style);
            //n.x += widthlabel.PreferredWidth;
            n.way = ar[0].way;
            n.x = 0;
            n.y = 0;
            n.text = ar[0].text;
            n.assum = false;
            array.Add(n);
            ar.RemoveAt(0);
            way = "0";
            for (; ar.Count != 0;)
            {
                way += "1";
                st_find = find_el(ar, way);
                if (st_find.fst)
                {
                    n.x += _shift_x;
                    n.y += _shift_y;
                    n.way = way;
                    n.text = ar[st_find.snd].text;
                    ar.RemoveAt(st_find.snd);
                    Console.Write("c = ");
                    Console.WriteLine(n.text);
                    array.Add(n);
                }
                else
                {
                    k = find_0(ar, array);
                    if (k != -1)
                    {
                        way = ar[k].way;
                        foreach (array_l _ar in array)
                        {
                            if (_ar.way == Term2.sinit2(way) + "1")
                            {
                                n.y += _shift_y;
                                n.way = way;
                                n.text = find_way(ar, way);
                                n.x = _ar.x;
                            }
                        }
                        Console.Write("c = ");
                        Console.WriteLine(n.text);
                        array.Add(n);
                        ar.RemoveAt(k);
                    }
                }
            }
            return array;
        }
        public st_find_el find_el(List<array_l> ar, String el)
        {
            st_find_el _el;
            for (int i = 0; i < ar.Count; i++)
            {
                if (ar[i].way == el)
                {
                    _el.fst = true;
                    _el.snd = i;
                    return _el;
                }
            }
            _el.fst = false;
            _el.snd = -1;
            return _el;
        }
        public int find_max(List<array_l> ar)
        {
            int max = 0;
            foreach (array_l l in ar)
            {
                if (l.way.Length > max)
                    max = l.way.Length;
            }
            return max;
        }
        public int find_0(List<array_l> ar, List<array_l> ar2)
        {
            int max = 0;
            int ind = -1;
            for (int i = 0; i < ar.Count; i++)
            {
                if (Term2.sslast(ar[i].way) == "0")
                {
                    if (ar[i].way.Length > max && _check_(ar2, (Term2.sinit2(ar[i].way) + "1")))
                    {
                        ind = i;
                        max = ar[i].way.Length;
                    }
                }
            }
            return ind;
        }
        public String find_way(List<array_l> ar, String str)
        {
            foreach (array_l l in ar)
            {
                if (l.way == str)
                    return l.text;
            }
            return "-1";
        }
        public Boolean check_ar(List<array_l> ar)
        {
            Label widthlabel = new Label();
            array_l _ar;
            widthlabel.Font = new Font(widthlabel.Font.Name, 12, widthlabel.Font.Style);
            //n.x += widthlabel.PreferredWidth;
            for (int i = 0; i < ar.Count; i++)
            {
                widthlabel.Text = ar[i].text;
                if (check_y(ar, ar[i].x, ar[i].x + widthlabel.PreferredWidth, ar[i].y, ar[i].way))
                    return true;
            }
            return false;
        }
        public Boolean _check_(List<array_l> ar, string str)
        {
            foreach (array_l l in ar)
            {
                if (l.way == str)
                    return true;
            }
            return false;
        }
        public bool check_y(List<array_l> ar, int _x, int x, int y, string str)
        {
            for (int i = 0; i < ar.Count; i++)
                if (ar[i].x <= x && ar[i].x >= _x && ar[i].way != str && ar[i].y == y)
                    return true;
            return false;
        }
        public array_l ret_el(List<array_l> ar, String way)
        {
            array_l n;
            String h;
            n.assum = false;
            n.text = "False";
            n.way = "False";
            n.x = -1;
            n.y = -1;
            foreach (array_l l in ar)
            {

                if (l.way == way)
                    return l;
            }
            return n;
        }
        public List<array_l> sort_ar(List<array_l> ar, List<array_l> ar2)
        {
            String way = "0";
            List<array_l> _ar = new List<array_l>();
            foreach (array_l l in ar2)
            {
                way = l.way;
                if (Term2.sslast(l.way) == "s")
                {
                    way = Term2.sinit2(l.way) + "1";
                }
                _ar.Add(ret_el(ar, way));
            }
            return _ar;
        }
        private void Form1_FormClosed(object sender, FormClosedEventArgs e)
        {
            Application.Exit();
        }

        private void проверкаТестовыхПримеровToolStripMenuItem_Click(object sender, EventArgs e)
        {
            Form3 testform = new Form3();
            testform.Show();
        }
        private void checkBox2_Click(object sender, EventArgs e)
        {
            checkBox1.Checked = false;
        }

        private void checkBox1_Click(object sender, EventArgs e)
        {
            checkBox2.Checked = false;
        }

        private void helpToolStripMenuItem_Click(object sender, EventArgs e)
        {
            String path = Directory.GetCurrentDirectory() + @"\help.doc";
            Process.Start(path);
        }

        private void toolStripTextBox1_Click(object sender, EventArgs e)
        {

        }
        private void abc1 ()
        {
            

        }

        private void оПрограммеToolStripMenuItem_Click(object sender, EventArgs e)
        {
            Form6 form6 = new Form6();
            form6.Show();
        }
    }
}
