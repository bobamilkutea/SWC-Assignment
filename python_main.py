

import json
import subprocess
import sys
from typing import List, Dict, Optional
import pandas as pd


class Student:
    """
    Student class
    """
    
    def __init__(self, student_id: int, name: str, program: str, 
                 completed_courses: List[str], grades: Dict[str, float],
                 average: Optional[float] = None,
                 distinction_status: Optional[bool] = None,
                 graduation_eligibility: Optional[bool] = None,
                 recommended_courses: Optional[List[str]] = None):
        """
        Initialize a Student object.
        
        Args:
            student_id: Unique student identifier
            name: Student's name
            program: Program code (e.g., "CS", "IT")
            completed_courses: List of completed course codes
            grades: Dictionary mapping course codes to grades
            average: Average grade (computed by Haskell)
            distinction_status: Whether student has distinction (computed by Haskell)
            graduation_eligibility: Whether student is eligible for graduation (computed by Prolog)
            recommended_courses: List of recommended courses (computed by Prolog)
        """
        self.id = student_id
        self.name = name
        self.program = program
        self.completed_courses = completed_courses
        self.grades = grades
        self.average = average
        self.distinction_status = distinction_status
        self.graduation_eligibility = graduation_eligibility
        self.recommended_courses = recommended_courses or []
    
    def predict_performance(self) -> str:
        """
        Predict student performance based on average grade.
        
        Returns:
            Performance category: "Excellent", "Good", "Average", or "At Risk"
        """
        if self.average is None:
            return "N/A"
        
        if self.average >= 85:
            return "Excellent"
        elif self.average >= 70:
            return "Good"
        elif self.average >= 50:
            return "Average"
        else:
            return "At Risk"
    
    def __repr__(self) -> str:
        """String representation of Student object."""
        return f"Student(id={self.id}, name={self.name}, program={self.program}, average={self.average})"


class UniversitySystem:
    """
    Main system class that integrates Haskell and Prolog engines.
    Demonstrates multi-paradigm programming in Python.
    """
    
    def __init__(self, database_path: str = "main_database.json"):
        """
        Initialize the University System.
        
        Args:
            database_path: Path to the main database JSON file
        """
        self.database_path = database_path
        self.students: List[Student] = []
        self.database_data: Dict = {}
    
    def load_database(self) -> Dict:
        """
        Read and load data from main_database.json.
        
        Returns:
            Dictionary containing students, programs, and courses data
        """
        try:
            with open(self.database_path, 'r', encoding='utf-8') as f:
                self.database_data = json.load(f)
            print(f"[OK] Successfully loaded database from {self.database_path}")
            return self.database_data
        except FileNotFoundError:
            print(f"[ERROR] {self.database_path} not found!")
            sys.exit(1)
        except json.JSONDecodeError as e:
            print(f"[ERROR] Invalid JSON format in {self.database_path}: {e}")
            sys.exit(1)
    
    def call_haskell_engine(self) -> Dict:
        """
        Call Haskell program (grades.hs) using subprocess.
        Haskell computes averages and identifies distinction students.
        
        Returns:
            Dictionary containing student averages and distinction status
        """
        try:
            print("\n[CALLING] Calling Haskell engine (grades.hs)...")
            result = subprocess.run(
                ["runhaskell", "grades.hs"],
                capture_output=True,
                text=True,
                check=True
            )
            
            # Parse JSON output from Haskell
            haskell_output = json.loads(result.stdout)
            print("[OK] Haskell engine executed successfully")
            return haskell_output
            
        except FileNotFoundError:
            print("[WARNING] Haskell program (grades.hs) not found or runhaskell not in PATH")
            print("  Returning empty results. Ensure grades.hs exists and Haskell is installed.")
            return {}
        except subprocess.CalledProcessError as e:
            print(f"[WARNING] Haskell execution failed: {e.stderr}")
            return {}
        except json.JSONDecodeError as e:
            print(f"[WARNING] Failed to parse Haskell output: {e}")
            return {}
    
    def call_prolog_engine(self) -> Dict:
        """
        Call Prolog program (advisory.pl) using subprocess.
        Prolog checks eligibility and recommends courses.
        
        Returns:
            Dictionary containing graduation eligibility and recommended courses
        """
        try:
            print("\n[CALLING] Calling Prolog engine (advisory.pl)...")
            result = subprocess.run(
                ["swipl", "-f", "advisory.pl", "-g", "run_query,halt."],
                capture_output=True,
                text=True,
                check=True
            )
            
            # Parse JSON output from Prolog
            prolog_output = json.loads(result.stdout)
            print("[OK] Prolog engine executed successfully")
            return prolog_output
            
        except FileNotFoundError:
            print("[WARNING] Prolog program (advisory.pl) not found or swipl not in PATH")
            print("  Returning empty results. Ensure advisory.pl exists and SWI-Prolog is installed.")
            return {}
        except subprocess.CalledProcessError as e:
            print(f"[WARNING] Prolog execution failed: {e.stderr}")
            return {}
        except json.JSONDecodeError as e:
            print(f"[WARNING] Failed to parse Prolog output: {e}")
            return {}
    
    def merge_results(self, haskell_results: Dict, prolog_results: Dict) -> List[Student]:
        """
        Merge base data, Haskell results, and Prolog results.
        Match students by ID and create Student objects.
        
        Args:
            haskell_results: Results from Haskell engine
            prolog_results: Results from Prolog engine
            
        Returns:
            List of Student objects with merged data
        """
        students = []
        
        # Create a mapping of student ID to results
        haskell_map = {}
        if "students" in haskell_results:
            for student_data in haskell_results["students"]:
                student_id = student_data.get("id")
                haskell_map[student_id] = student_data
        
        prolog_map = {}
        if "students" in prolog_results:
            for student_data in prolog_results["students"]:
                student_id = student_data.get("id")
                prolog_map[student_id] = student_data
        
        # Merge base data with Haskell and Prolog results
        for base_student in self.database_data.get("students", []):
            student_id = base_student["id"]
            
            # Get Haskell results for this student
            haskell_data = haskell_map.get(student_id, {})
            average = haskell_data.get("average")
            distinction_status = haskell_data.get("distinction", False)
            
            # Get Prolog results for this student
            prolog_data = prolog_map.get(student_id, {})
            graduation_eligibility = prolog_data.get("graduation_eligible", False)
            recommended_courses = prolog_data.get("recommended_courses", [])
            
            # Create Student object
            student = Student(
                student_id=student_id,
                name=base_student["name"],
                program=base_student["program"],
                completed_courses=base_student["completed_courses"],
                grades=base_student["grades"],
                average=average,
                distinction_status=distinction_status,
                graduation_eligibility=graduation_eligibility,
                recommended_courses=recommended_courses
            )
            
            students.append(student)
        
        self.students = students
        return students
    
    def initialize_system(self):
        """Initialize the system by loading data and calling external engines."""
        # Load database
        self.load_database()
        
        # Call Haskell engine
        haskell_results = self.call_haskell_engine()
        
        # Call Prolog engine
        prolog_results = self.call_prolog_engine()
        
        # Merge results
        self.merge_results(haskell_results, prolog_results)
        
        print(f"\n[OK] System initialized with {len(self.students)} students")
    
    # Functional Programming Operations
    
    def get_distinction_students(self) -> List[Student]:
        """
        Identify all distinction students using filter (Functional Programming).
        
        Returns:
            List of students with distinction status
        """
        return list(filter(lambda s: s.distinction_status is True, self.students))
    
    def get_at_risk_students(self) -> List[Student]:
        """
        Identify at-risk students using filter and lambda (Functional Programming).
        At-risk students have average < 50 or no average computed.
        
        Returns:
            List of at-risk students
        """
        return list(filter(
            lambda s: s.average is not None and s.average < 50,
            self.students
        ))
    
    def generate_performance_predictions(self) -> List[Dict]:
        """
        Generate performance predictions for all students using map (Functional Programming).
        
        Returns:
            List of dictionaries with student ID and predicted performance
        """
        return list(map(
            lambda s: {"id": s.id, "name": s.name, "prediction": s.predict_performance()},
            self.students
        ))
    
    def rank_students_by_average(self) -> List[Student]:
        """
        Rank students by average grade using sorted and lambda (Functional Programming).
        
        Returns:
            List of students sorted by average (descending)
        """
        return sorted(
            self.students,
            key=lambda s: s.average if s.average is not None else -1,
            reverse=True
        )
    
    def get_top_performer(self) -> Optional[Student]:
        """
        Get the top-performing student using functional programming.
        
        Returns:
            Student with highest average, or None if no students
        """
        ranked = self.rank_students_by_average()
        return ranked[0] if ranked and ranked[0].average is not None else None
    
    # Pandas Integration
    
    def create_dataframe(self) -> pd.DataFrame:
        """
        Create a pandas DataFrame with student information.
        
        Returns:
            DataFrame containing student data
        """
        data = []
        for student in self.students:
            data.append({
                "ID": student.id,
                "Name": student.name,
                "Program": student.program,
                "Average": student.average if student.average is not None else "N/A",
                "Distinction": "Yes" if student.distinction_status else "No",
                "Graduation Eligible": "Yes" if student.graduation_eligibility else "No",
                "Recommended Courses": ", ".join(student.recommended_courses) if student.recommended_courses else "None",
                "Predicted Performance": student.predict_performance()
            })
        
        df = pd.DataFrame(data)
        return df
    
    def display_dashboard(self):
        """Display a formatted dashboard using pandas DataFrame."""
        df = self.create_dataframe()
        print("\n" + "="*100)
        print("STUDENT DASHBOARD".center(100))
        print("="*100)
        print(df.to_string(index=False))
        print("="*100 + "\n")
    
    # Console Interface
    
    def view_all_students(self):
        """Display all students in a formatted table."""
        self.display_dashboard()
    
    def check_specific_student(self, student_id: int):
        """
        Display detailed information for a specific student.
        
        Args:
            student_id: ID of the student to display
        """
        student = next((s for s in self.students if s.id == student_id), None)
        
        if student:
            print("\n" + "="*60)
            print(f"STUDENT DETAILS - ID: {student.id}".center(60))
            print("="*60)
            print(f"Name: {student.name}")
            print(f"Program: {student.program}")
            print(f"Completed Courses: {', '.join(student.completed_courses)}")
            print(f"Grades: {student.grades}")
            print(f"Average: {student.average if student.average is not None else 'N/A'}")
            print(f"Distinction Status: {'Yes' if student.distinction_status else 'No'}")
            print(f"Graduation Eligible: {'Yes' if student.graduation_eligibility else 'No'}")
            print(f"Recommended Courses: {', '.join(student.recommended_courses) if student.recommended_courses else 'None'}")
            print(f"Predicted Performance: {student.predict_performance()}")
            print("="*60 + "\n")
        else:
            print(f"\n[ERROR] Student with ID {student_id} not found.\n")
    
    def show_top_performer(self):
        """Display the top-performing student."""
        top_student = self.get_top_performer()
        
        if top_student:
            print("\n" + "="*60)
            print("TOP PERFORMER".center(60))
            print("="*60)
            print(f"ID: {top_student.id}")
            print(f"Name: {top_student.name}")
            print(f"Program: {top_student.program}")
            print(f"Average: {top_student.average}")
            print(f"Distinction: {'Yes' if top_student.distinction_status else 'No'}")
            print("="*60 + "\n")
        else:
            print("\n[ERROR] No top performer found (no students with computed averages).\n")
    
    def show_at_risk_students(self):
        """Display all at-risk students."""
        at_risk = self.get_at_risk_students()
        
        if at_risk:
            print("\n" + "="*60)
            print(f"AT-RISK STUDENTS ({len(at_risk)})".center(60))
            print("="*60)
            for student in at_risk:
                print(f"ID: {student.id} | Name: {student.name} | Average: {student.average}")
            print("="*60 + "\n")
        else:
            print("\n[OK] No at-risk students found.\n")
    
    def run_menu(self):
        """Run the main console interface menu."""
        while True:
            print("\n" + "="*60)
            print("SMART UNIVERSITY MANAGEMENT SYSTEM".center(60))
            print("="*60)
            print("1. View All Students")
            print("2. Check Specific Student")
            print("3. Show Top Performer")
            print("4. Show At-Risk Students")
            print("5. Exit")
            print("="*60)
            
            choice = input("\nEnter your choice (1-5): ").strip()
            
            if choice == "1":
                self.view_all_students()
            elif choice == "2":
                try:
                    student_id = int(input("Enter student ID: "))
                    self.check_specific_student(student_id)
                except ValueError:
                    print("\n[ERROR] Invalid input. Please enter a valid student ID.\n")
            elif choice == "3":
                self.show_top_performer()
            elif choice == "4":
                self.show_at_risk_students()
            elif choice == "5":
                print("\n[OK] Thank you for using Smart University Management System!")
                break
            else:
                print("\n[ERROR] Invalid choice. Please enter a number between 1 and 5.\n")


def main():
    """Main entry point of the program."""
    print("\n" + "="*60)
    print("Initializing Smart University Management System...".center(60))
    print("="*60)
    
    # Create system instance
    system = UniversitySystem("main_database.json")
    
    # Initialize system (loads data and calls external engines)
    system.initialize_system()
    
    # Run interactive menu
    system.run_menu()


if __name__ == "__main__":
    main()
