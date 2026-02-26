# Smart University Management System - Streamlit Web Interface

## 🚀 Quick Start

### 1. Install Dependencies

```bash
pip install -r requirements.txt
```

This will install:
- Streamlit (Python-only web framework - NO HTML needed!)
- pandas (data processing)

### 2. Run the Web Application

```bash
streamlit run app.py
```

The server will automatically open in your browser at **http://localhost:8501**

## ✨ Features

### ✅ Dashboard
- View all students in a beautiful table
- See statistics (total students, distinction count, etc.)
- View detailed student information
- All displayed using Python code only!

### ✅ Add Student
- Simple form to add new students
- Validates input automatically
- Saves to JSON database
- Automatically processes through Haskell/Prolog

### ✅ Top Performer
- Displays the student with highest average
- Beautiful highlight card
- Shows all relevant metrics

### ✅ At-Risk Students
- Lists students with average < 50
- Expandable details for each student
- Helps identify students needing attention

## 🎯 Why Streamlit?

- **100% Python** - No HTML, CSS, or JavaScript needed!
- **Simple** - Just write Python code, Streamlit handles the UI
- **Beautiful** - Modern, responsive interface automatically
- **Fast** - Easy to build and deploy
- **Interactive** - Forms, buttons, charts all in Python

## 🗄️ Database

**The JSON database (`main_database.json`) is a real database!**

- When you add a student, it's **saved to the JSON file**
- Data persists between runs
- Both web app and terminal version can use the same file

## 📝 How It Works

1. **Run:** `streamlit run app.py`
2. **Browser opens automatically** at http://localhost:8501
3. **Navigate** using the sidebar menu
4. **Add students** using the form
5. **View data** in beautiful tables and cards

## 🛠️ Troubleshooting

**Port already in use?**
- Streamlit will automatically try the next port (8502, 8503, etc.)
- Or specify: `streamlit run app.py --server.port 8502`

**Module not found?**
- Make sure you're in the project directory
- Run: `pip install -r requirements.txt`

**Haskell/Prolog not found?**
- App still works, but averages/eligibility won't compute
- Check that `grades.hs` and `PrologAssignment.pl` are in the directory

## 💡 Example Usage

```bash
# Install dependencies
pip install -r requirements.txt

# Run the app
streamlit run app.py

# Browser opens automatically!
# Use sidebar to navigate between pages
# Add students, view dashboard, check top performer
```

## 🎨 All Python, No HTML!

Everything you see is created with Python code:
- `st.title()` - Creates titles
- `st.dataframe()` - Creates tables
- `st.form()` - Creates forms
- `st.metric()` - Creates metrics
- `st.columns()` - Creates layouts

No HTML templates needed!
