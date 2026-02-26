# Part C - Python Integration Layer

## Overview

This Python integration layer (`python_main.py`) serves as the multi-paradigm interface for the Smart University Management System, integrating Haskell (functional programming) and Prolog (logic programming) engines.

## Features

✅ **Object-Oriented Programming**: Student class with encapsulation and methods  
✅ **Functional Programming**: Uses map, filter, lambda, and list comprehensions  
✅ **System Integration**: Calls Haskell and Prolog engines via subprocess  
✅ **Data Visualization**: Pandas DataFrame for structured display  
✅ **Interactive Console**: Menu-driven interface for system interaction  

## Installation

1. Install Python dependencies:
```bash
pip install -r requirements.txt
```

2. Ensure Haskell and Prolog are installed:
   - Haskell: `runhaskell` command should be available
   - Prolog: `swipl` command should be available (SWI-Prolog)

3. Ensure the following files exist:
   - `main_database.json` - Main database (already created)
   - `grades.hs` - Haskell program (functional grade engine)
   - `PrologAssignment.pl` - Prolog program (graduation & advisory engine)

## Usage

Run the Python integration layer:

```bash
python python_main.py
```

The system will:
1. Load `main_database.json`
2. Call Haskell engine (`runhaskell grades.hs`)
3. Call Prolog engine (`swipl -f PrologAssignment.pl -g run_query,halt.`)
4. Merge results and display interactive menu

## Menu Options

1. **View All Students** - Display all students in a formatted table
2. **Check Specific Student** - View detailed information for a student by ID
3. **Show Top Performer** - Display the student with highest average
4. **Show At-Risk Students** - List students with average < 50
5. **Exit** - Close the application

## Expected JSON Formats

### Haskell Output (`grades.hs`)
```json
{
  "students": [
    {
      "id": 101,
      "average": 81.5,
      "distinction": true
    }
  ],
  "top_student": {
    "id": 102,
    "name": "Siti"
  }
}
```

### Prolog Output (`PrologAssignment.pl`)
```json
{
  "students": [
    {
      "id": 101,
      "graduation_eligible": false,
      "recommended_courses": ["CS201"]
    }
  ]
}
```

## Architecture

```
Python (python_main.py)
    ↓
    ├──→ Haskell (grades.hs) - Grade computations
    ├──→ Prolog (PrologAssignment.pl) - Eligibility logic
    └──→ main_database.json - Shared database
```

## Code Structure

- **Student Class**: OOP representation of student data
- **UniversitySystem Class**: Main system orchestrator
- **Functional Operations**: map, filter, lambda for data processing
- **Integration Methods**: Subprocess calls to external engines
- **Display Methods**: Pandas DataFrame and console output

## Notes

- The system gracefully handles missing Haskell/Prolog programs with warnings
- All data merging is done by matching student IDs
- Performance predictions are computed in Python using the `predict_performance()` method
- The system does NOT compute averages or check graduation rules (those are handled by Haskell and Prolog respectively)
