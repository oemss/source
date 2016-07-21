using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Globalization;
using System.Linq;
using System.Diagnostics;
using System.Text;
using System.IO;
using TypeLib;
using Library2;
using System.Windows.Forms;

namespace WindowsFormsApplication2
{
    public partial class Form3 : Form
    {
        public Form3()
        {
            InitializeComponent();
        }
        int j = 0;
        private void button1_Click(object sender, EventArgs e)
        {
            try
            {
                String line;
                int i;
                String path = Directory.GetCurrentDirectory() + "\\Terms.txt";
                if (File.Exists(path))
                {
                    StreamReader fs = new StreamReader(@path);
                    while ((line = fs.ReadLine()) != null)
                    {
                        if (line.IndexOf('.') != -1)
                        {
                            for (i = 0; line[i] != '.'; i++) ;
                            line = line.Substring(i + 1);
                            textBox1.Text += "test" + j.ToString() + " :: " + Term2.deftype(line) + Environment.NewLine;
                            textBox1.Text += "test" + j.ToString() + " = " + Term2.show(Term2.sterm(line)) + Environment.NewLine;
                            j++;
                        }
                    }
                    fs.Close();
                }
                else
                {
                    using (FileStream fs = File.Create(path))
                    {
                        Byte[] info = new UTF8Encoding(true).GetBytes("1.\\f.\\x.f x\n2.\\f.\\x.f (f x)\n3.\\f.\\x.f (f x)\n4.\\x.x x\n5.\\x.x x x\n6.\\x.x x x x\n7.\\x.\\y.x y x\n8.\\x.\\y.x y y\n9.\\f.\\x.f (x x)\n10.\\x.x (\\z.z z)\n11.\\x.(x x) (x x) x\n14.(\\x.x) (\\x.x)\n16.\\x.\\y.\\z.x z y\n17.\\x.x (x (x (\\z.z z)))\n");
                        fs.Write(info, 0, info.Length);
                        fs.Close();
                    }
                }
            }
            catch
            {
                textBox1.Text = "Ошибка";
            }
        }

        private void button3_Click(object sender, EventArgs e)
        {
            textBox1.Text = "";
            textBox3.Text = "";
        }

        private void button2_Click(object sender, EventArgs e)
        {
            try
            {
                String line;
                int i;
                String path = Directory.GetCurrentDirectory() + "\\Terms.txt";
                if (File.Exists(path))
                {
                    StreamReader fs = new StreamReader(@path);
                    int k = textBox3.Text.Length;
                    line = fs.ReadLine();
                    for (; line.Substring(0, k) != textBox3.Text;)
                    {
                        if ((line = fs.ReadLine()) != null)
                            line = line;
                    }
                    if (line != null)
                    {
                        for (i = 0; line[i] != '.'; i++) ;
                        line = line.Substring(i + 1);
                        textBox1.Text += "test" + j.ToString() + " :: " + Term2.deftype(line) + Environment.NewLine;
                        textBox1.Text += "test" + j.ToString() + " = " + Term2.show(Term2.sterm(line)) + Environment.NewLine;
                        j++;
                    }
                    fs.Close();
                }
            }
            catch
            {
                textBox1.Text = "Ошибка";
            }
        }

        private void button4_Click(object sender, EventArgs e)
        {
            try
            {
                openFileDialog1.ShowDialog();
                String line;
                int i;
                String path = openFileDialog1.FileName;
                if (File.Exists(path))
                {
                    StreamReader fs = new StreamReader(@path);
                    while ((line = fs.ReadLine()) != null)
                    {
                        if (line.IndexOf('.') != -1)
                        {
                            for (i = 0; line[i] != '.'; i++) ;
                            line = line.Substring(i + 1);
                            textBox1.Text += "test" + j.ToString() + " :: " + Term2.deftype(line) + Environment.NewLine;
                            textBox1.Text += "test" + j.ToString() + " = " + Term2.show(Term2.sterm(line)) + Environment.NewLine;
                            j++;
                        }
                    }
                    fs.Close();
                }
            }
            catch
            {
                textBox1.Text = "Ошибка";
            }
        }

        private void button5_Click(object sender, EventArgs e)
        {
            try {
                String path = Directory.GetCurrentDirectory() + @"\winghci\winghci.exe";
                DateTime localDate = DateTime.Now;
                
                
                String path2 = Directory.GetCurrentDirectory() + @"\New(" + localDate.Ticks.ToString() + ").hs";
                if (openFileDialog1.FileName != "openFileDialog1")
                {
                    path2 = openFileDialog1.FileName.Replace(".txt","(" + localDate.Ticks.ToString() + ").hs");
                }
                Console.WriteLine(openFileDialog1.FileName);
                String lines = "{-# LANGUAGE RankNTypes #-}\n" + textBox1.Text;
                if (File.Exists(path2))
                {
                    File.WriteAllText(path2,"");
                    System.IO.File.WriteAllText(path2, lines);
                }
                else
                {
                    FileStream fs = File.Create(path2);
                    fs.Close();
                    System.IO.File.WriteAllText(path2, lines);
                }
            Console.WriteLine(path);
            Console.WriteLine(path2);
                Process.Start(path, path2);
            }
            catch { }

        }

        private void Form3_FormClosed(object sender, FormClosedEventArgs e)
        {
            //Form2 form2 = new Form2();
            //form2._vis();
        }
    }
}
