using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Windows.Forms;
using System.Diagnostics;

namespace WindowsFormsApplication2
{
    public partial class Form2 : Form
    {
        public Form2()
        {
            InitializeComponent();
        }

        private void but_tsp_Click(object sender, EventArgs e)
        {
            Form1 formTSP = new Form1();
            formTSP.Show();
            //this.Visible = false;
        }

        private void starttest_Click(object sender, EventArgs e)
        {
            Form3 formTest = new Form3();
            formTest.Show();
            //this.Visible = false;
        }

        private void button3_Click(object sender, EventArgs e)
        {
            Form5 formlw = new Form5();
            formlw.Show();
            //this.Visible = false;
        }
        public void _vis()
        {
            //this.Visible = true;
        }
    }
}
