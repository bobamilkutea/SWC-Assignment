# Comprehensive Setup Guide: Smart University Management System

This guide provides detailed instructions for setting up the development environment for the Smart University Management System. This project integrates Python, Haskell, and Prolog to demonstrate multi-paradigm programming.

## 1. Prerequisites

Before you begin, ensure you have the following software installed on your system.

### 1.1 Python
- **Required Version**: Python 3.10 or higher
- **Download**: [python.org](https://www.python.org/downloads/)
- **Verification**: Run `python --version` in your terminal.

### 1.2 Haskell Platform (GHC)
- **Required Component**: `runhaskell` (part of GHC)
- **Download**: [haskell.org/ghcup](https://www.haskell.org/ghcup/) (Recommended) or install via your OS package manager.
- **Verification**: Run `runhaskell --version` in your terminal.

### 1.3 SWI-Prolog
- **Required Component**: `swipl` command-line tool
- **Download**: [swi-prolog.org](https://www.swi-prolog.org/Download.html)
- **Note**: On Windows, ensure you select "Add swipl to the system PATH for all users" (or current user) during installation.
- **Verification**: Run `swipl --version` in your terminal.

### 1.4 Git
- **Download**: [git-scm.com](https://git-scm.com/downloads)
- **Verification**: Run `git --version` in your terminal.

---

## 2. Cloning the Repository

Open your terminal or command prompt and run the following commands:

```bash
# Clone the repository
git clone https://github.com/YourUsername/SWC-Assignment.git

# Navigate into the project directory
cd SWC-Assignment
```

---

## 3. Environment Setup

It is highly recommended to use a virtual environment to manage Python dependencies.

### 3.1 Create a Virtual Environment

**Windows:**
```powershell
python -m venv venv
```

**macOS / Linux:**
```bash
python3 -m venv venv
```

### 3.2 Activate the Virtual Environment

**Windows (Command Prompt):**
```cmd
venv\Scripts\activate
```

**Windows (PowerShell):**
```powershell
.\venv\Scripts\Activate.ps1
```

**macOS / Linux:**
```bash
source venv/bin/activate
```

*You should see `(venv)` appear at the start of your terminal prompt.*

### 3.3 Install Dependencies

With the virtual environment activated, install the required Python packages:

```bash
pip install -r requirements.txt
```

This will install:
- `streamlit` (for the web interface)
- `pandas` (for data manipulation and display)

---

## 4. Database Setup

The system uses a local JSON file (`main_database.json`) as its database.

- **Status**: The repository comes with a pre-populated `main_database.json` file.
- **Action**: No manual setup is required. The application will automatically read from and write to this file.
- **Reset**: If you need to reset the data, you can replace `main_database.json` with a backup or manually edit the JSON structure.

---

## 5. Running the Application

There are two ways to interact with the system: the modern Web Interface and the classic CLI Interface.

### 5.1 Option A: Web Interface (Recommended)

This launches the Streamlit-based dashboard.

```bash
streamlit run app.py
```

- The application should automatically open in your default web browser at `http://localhost:8501`.
- If it doesn't open automatically, copy the URL displayed in the terminal and paste it into your browser.
- **Features**: Dashboard view, Add Student form, visual statistics, and interactive data tables.

### 5.2 Option B: CLI Interface

This launches the command-line version of the application.

```bash
python python_main.py
```

- Follow the on-screen menu prompts to view students, check specific records, or analyze performance.

---

## 6. Troubleshooting

### "runhaskell: command not found" or "swipl: command not found"
- **Cause**: Haskell or Prolog is not installed or not added to your system's PATH.
- **Fix**: Reinstall the respective software and ensure the "Add to PATH" option is checked. Restart your terminal after installation.

### "ModuleNotFoundError: No module named 'streamlit'"
- **Cause**: Dependencies are not installed in the current environment.
- **Fix**: Ensure your virtual environment is activated (step 3.2) and run `pip install -r requirements.txt` again.

### "JSONDecodeError"
- **Cause**: The `main_database.json` file might be corrupted or empty.
- **Fix**: Check the file content. It should be valid JSON structure starting with `{ "students": [ ... ] }`.

### "Permission denied" on Windows
- **Cause**: Script execution policy might restrict the activation script.
- **Fix**: Open PowerShell as Administrator and run: `Set-ExecutionPolicy RemoteSigned`.

---

## 7. Development Notes

- **Modifying Logic**:
    - Edit `python_main.py` for core business logic.
    - Edit `grades.hs` to change how grades/distinctions are calculated.
    - Edit `PrologAssignment.pl` to change graduation eligibility rules.
- **Adding Dependencies**: If you install new Python packages, remember to update `requirements.txt`:
  ```bash
  pip freeze > requirements.txt
  ```
