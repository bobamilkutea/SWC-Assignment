

import json
import subprocess
import sys
from typing import List, Dict, Optional
import pandas as pd


class Student:
    def __init__(self, student_id: int, name: str, program: str,
                 completed_courses: List[str], grades: Dict[str, float],
                 average: Optional[float] = None,
                 distinction_status: Optional[bool] = None,
                 grad_eligibility: Optional[bool] = None,
                 recommended: Optional[List[str]] = None):
        """
        Python constructor.
        Delegates to the explicit init method so we can also expose a
        method literally named 'init' if required by the assignment.
        """
        self.init(
            student_id,
            name,
            program,
            completed_courses,
            grades,
            average,
            distinction_status,
            grad_eligibility,
            recommended,
        )

    def init(self, student_id: int, name: str, program: str,
             completed_courses: List[str], grades: Dict[str, float],
             average: Optional[float] = None,
             distinction_status: Optional[bool] = None,
             grad_eligibility: Optional[bool] = None,
             recommended: Optional[List[str]] = None):
        """Simple container for one student's data."""
        self.id = student_id
        self.name = name
        self.program = program
        self.completed_courses = completed_courses
        self.grades = grades
        self.average = average
        self.distinction_status = distinction_status
        self.graduation_eligibility = grad_eligibility
        self.recommended_courses = recommended or []
    
    def predict_performance(self) -> str:
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
        return f"Student(id={self.id}, name={self.name}, program={self.program}, avg={self.average})"

    def repr(self) -> str:
        """Alias for __repr__ so a method literally named 'repr' exists."""
        return self.__repr__()


class UniversitySystem:
    def __init__(self, database_path: str = "main_database.json"):
        self.database_path = database_path
        self.students: List[Student] = []
        self.database_data: Dict = {}
    
    def load_database(self) -> Dict:
        """
        Read and load data from database.
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
    
    def save_database(self):
        """
        Save current database data to JSON file.
        """
        try:
            with open(self.database_path, 'w', encoding='utf-8') as f:
                json.dump(self.database_data, f, indent=2, ensure_ascii=False)
            print(f"[OK] Successfully saved database to {self.database_path}")
        except Exception as e:
            print(f"[ERROR] Failed to save database: {e}")
            raise
    
    def add_student_to_database(self, student_id: int, name: str, program: str, 
                                completed_courses: List[str], grades: Dict[str, float]):
        """
        Add a new student to the JSON database.
        """
        # Reload database to get latest data
        self.load_database()
        
        # Create new student entry
        new_student = {
            "id": student_id,
            "name": name,
            "program": program,
            "completed_courses": completed_courses,
            "grades": grades
        }
        
        # Add to students list
        if "students" not in self.database_data:
            self.database_data["students"] = []
        
        self.database_data["students"].append(new_student)
        
        # Save to file
        self.save_database()

    def update_student_courses(
        self,
        student_id: int,
        new_completed_courses: List[str],
        new_grades: Dict[str, float],
    ):
        """
        Update an existing student's completed courses and grades in the JSON database.
        Used when a student completes new courses to recalculate averages.
        """
        self.load_database()

        students_list = self.database_data.get("students", [])
        target_student = next((s for s in students_list if s.get("id") == student_id), None)

        if not target_student:
            raise ValueError(f"Student with ID {student_id} not found in database.")

        # Merge completed courses (avoid duplicates)
        existing_courses = set(target_student.get("completed_courses", []))
        for course in new_completed_courses:
            course_code = course.strip()
            if course_code and course_code not in existing_courses:
                existing_courses.add(course_code)

        target_student["completed_courses"] = sorted(existing_courses)

        # Merge grades (overwrite or add new)
        existing_grades = target_student.get("grades", {})
        for course, grade in new_grades.items():
            existing_grades[course] = grade

        target_student["grades"] = existing_grades

        # Save updated database
        self.save_database()
    
    def init(self, database_path: str = "main_database.json"):
        """
        Optional initializer named 'init' for compatibility with
        assignment naming, delegates to __init__.
        """
        self.__init__(database_path)
    
    def call_haskell_engine(self) -> Dict:
        """
        Call Haskell
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
            
            return {}
        except subprocess.CalledProcessError as e:
            print(f"[WARNING] Haskell execution failed: {e.stderr}")
            return {}
        except json.JSONDecodeError as e:
            print(f"[WARNING] Failed to parse Haskell output: {e}")
            return {}
    
    def callhaskell(self) -> Dict:
        """Alias so a method literally named 'callhaskell' is available."""
        return self.call_haskell_engine()
    
    def call_prolog_engine(self) -> Dict:
        """
        Call Prolog
        """
        try:
            print("\n[CALLING] Calling Prolog engine (PrologAssignment.pl)...")
            result = subprocess.run(
                ["swipl", "-f", "PrologAssignment.pl", "-g", "run_query,halt."],
                capture_output=True,
                text=True,
                check=True
            )
            
            # Parse JSON output from Prolog
            prolog_output = json.loads(result.stdout)
            print("[OK] Prolog engine executed successfully")
            return prolog_output
            
        except FileNotFoundError:
            print("[WARNING] Prolog not found")
            
            return {}
        except subprocess.CalledProcessError as e:
            print(f"[WARNING] Prolog execution failed: {e.stderr}")
            return {}
        except json.JSONDecodeError as e:
            print(f"[WARNING] Failed to parse Prolog output: {e}")
            return {}
    
    def callprolog(self) -> Dict:
        """Alias so a method literally named 'callprolog' is available."""
        return self.call_prolog_engine()
    
    def merge_results(self, haskell_results: Dict, prolog_results: Dict) -> List[Student]:
        """
        Merge all data from haskell and prolog and output result
        """
        students = []
        
        # Create a mapping of student ID to results
        haskellmap: Dict[int, Dict] = {}
        if "students" in haskell_results:
            for student_data in haskell_results["students"]:
                student_id = student_data.get("id")
                haskellmap[student_id] = student_data
        
        prolog_map: Dict[int, Dict] = {}
        if "students" in prolog_results:
            for student_data in prolog_results["students"]:
                student_id = student_data.get("id")
                prolog_map[student_id] = student_data
        
        # Merge base data with Haskell and Prolog results
        for base_student in self.database_data.get("students", []):
            student_id = base_student["id"]
            
            # Get Haskell results for this student
            haskell_data = haskellmap.get(student_id, {})
            average = haskell_data.get("average")
            distinction_status = haskell_data.get("distinction", False)
            
            # Get Prolog results for this student
            prolog_data = prolog_map.get(student_id, {})
            grad_eligibility = prolog_data.get("graduation_eligible", False)
            recommended = prolog_data.get("recommended_courses", [])
            
            # Create Student object
            student = Student(
                student_id=student_id,
                name=base_student["name"],
                program=base_student["program"],
                completed_courses=base_student["completed_courses"],
                grades=base_student["grades"],
                average=average,
                distinction_status=distinction_status,
                grad_eligibility=grad_eligibility,
                recommended=recommended
            )
            
            students.append(student)
        
        self.students = students
        return students
    
    def mergeresult(self, haskell_results: Dict, prolog_results: Dict) -> List[Student]:
        """Alias so a method literally named 'mergeresult' is available."""
        return self.merge_results(haskell_results, prolog_results)
    
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
    
    def distinct_students(self) -> List[Student]:
        """
        Identify all distinction students using filter

        """
        return list(filter(lambda s: s.distinction_status is True, self.students))
    
    def get_at_risk_students(self) -> List[Student]:
        """
        Identify at-risk students using filter and lambda
        At-risk students have average < 50 or no average computed.

        """
        return list(filter(
            lambda s: s.average is None or s.average < 50,
            self.students
        ))
    
    def perf_predictions(self) -> List[Dict]:
        """
        Generate performance predictions for all students using map
        """
        return list(map(
            lambda s: {"id": s.id, "name": s.name, "prediction": s.predict_performance()},
            self.students
        ))
    
    def rank_students_by_average(self) -> List[Student]:
        """
        Rank students by average grade using sorted and lambda
        """
        return sorted(
            self.students,
            key=lambda s: s.average if s.average is not None else -1,
            reverse=True
        )
    
    def get_top_performer(self) -> Optional[Student]:
        """
        Alias for top_perform so other parts of the code that expect
        get_top_performer continue to work.
        """
        return self.top_perform()
    
    def top_perform(self) -> Optional[Student]:
        """
        Get the top-performing student using functional programming.
        
        """
        ranked = self.rank_students_by_average()
        return ranked[0] if ranked and ranked[0].average is not None else None
    
    # Pandas Integration
    
    def dataframe(self) -> pd.DataFrame:
        """
        Create a pandas DataFrame with student information.
        
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
    
    def create_dataframe(self) -> pd.DataFrame:
        """Alias for dataframe to keep the original method name usable."""
        return self.dataframe()
    
    def dashboard(self):
        """Display a formatted dashboard using pandas DataFrame."""
        df = self.dataframe()
        print("\n" + "="*100)
        print("STUDENT DASHBOARD".center(100))
        print("="*100)
        print(df.to_string(index=False))
        print("="*100 + "\n")
    
    # Console Interface
    
    def display_dashboard(self):
        """Alias for dashboard to keep the original method name usable."""
        return self.dashboard()
    
    def all_students(self):
        """Display all students in a formatted table."""
        self.dashboard()
        input("\nPress Enter to return to the main menu...")
    
    def view_all_students(self):
        """Alias for all_students to keep the original method name usable."""
        return self.all_students()
    
    def check_specific(self, student_id: int):
        """
        Display detailed information for a specific student.

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
        
        input("\nPress Enter to return to the main menu...")
    
    def check_specific_student(self, student_id: int):
        """Alias for check_specific to keep the original method name usable."""
        return self.check_specific(student_id)
    
    def top_performer(self):
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
        
        input("\nPress Enter to return to the main menu...")
    
    def show_top_performer(self):
        """Alias for top_performer to keep the original method name usable."""
        return self.top_performer()
    
    def at_risk_students(self):
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
        
        input("\nPress Enter to return to the main menu...")
    
    def show_at_risk_students(self):
        """Alias for at_risk_students to keep the original method name usable."""
        return self.at_risk_students()
    
    def menu(self):
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
                self.all_students()
            elif choice == "2":
                try:
                    student_id = int(input("Enter student ID: "))
                    self.check_specific(student_id)
                except ValueError:
                    print("\n[ERROR] Invalid input. Please enter a valid student ID.\n")
            elif choice == "3":
                self.top_performer()
            elif choice == "4":
                self.at_risk_students()
            elif choice == "5":
                print("\n[OK] Thank you for using Smart University Management System!")
                break
            else:
                print("\n[ERROR] Invalid choice. Please enter a number between 1 and 5.\n")

    def run_menu(self):
        """Alias for menu to keep the original method name usable."""
        return self.menu()


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
