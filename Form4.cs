using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Windows.Forms;

namespace WindowsFormsApplication2
{
    public partial class Form4 : Form
    {
        public Form4()
        {
            InitializeComponent();
        }

        private void button1_Click(object sender, EventArgs e)
        {
            textBox1.Text = "";
            Form1 form1 = new Form1();
            //form1._press_off();
            form1._vis();
            this.Close();
        }
        public void location(int x, int y)
        {
            Point point = new Point(x / 2, y / 2);
            //this.Location = point;
        }
        public void label_change(String s)
        {
            textBox1.Text = s;
        }

        private void Form4_Load(object sender, EventArgs e)
        {
            this.ControlBox = false;

        }
    }
}
