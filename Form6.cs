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
    public partial class Form6 : Form
    {
        public Form6()
        {
            InitializeComponent();
            textBox1.Text = "Дата написания 1.11.2015-16.05.2016";
            textBox1.Text += Environment.NewLine + "Автор - Орлов Евгений Михайлович";
            textBox1.Text += Environment.NewLine;
            textBox1.Text += "Связь с автором - thispagenotfound@yandex.ru";
        }

        private void textBox1_TextChanged(object sender, EventArgs e)
        {

        }
    }
}
