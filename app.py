import streamlit as st
import json
from python_main import UniversitySystem, Student

# -------------------------------------------------
# Page Configuration
# -------------------------------------------------
st.set_page_config(
    page_title="Smart University Management System",
    page_icon="🎓",
    layout="wide",
    initial_sidebar_state="expanded"
)

# -------------------------------------------------
# 🌿 Mint Green Minimalist Theme
# -------------------------------------------------
st.markdown("""
<style>

/* Main Background */
.stApp {
    background-color: #f4fffb;
}

/* Sidebar */
section[data-testid="stSidebar"] {
    background-color: #e6fff7;
    border-right: 1px solid #d1f7eb;
}

/* Headings */
h1, h2, h3 {
    color: #0f766e;
    font-weight: 600;
}

/* Metric Cards */
[data-testid="metric-container"] {
    background-color: #ffffff;
    border-radius: 18px;
    padding: 18px;
    box-shadow: 0px 6px 18px rgba(0, 0, 0, 0.05);
}

/* Buttons */
.stButton>button {
    background-color: #34d399;
    color: white;
    border-radius: 12px;
    border: none;
    padding: 0.6rem 1rem;
    font-weight: 500;
}

.stButton>button:hover {
    background-color: #10b981;
}

/* Forms */
div[data-testid="stForm"] {
    background-color: #ffffff;
    padding: 30px;
    border-radius: 18px;
    box-shadow: 0px 8px 20px rgba(0, 0, 0, 0.05);
}

/* Expanders */
details {
    background-color: #ffffff;
    border-radius: 14px;
    padding: 12px;
    box-shadow: 0px 5px 15px rgba(0,0,0,0.04);
}

/* Dataframe */
[data-testid="stDataFrame"] {
    border-radius: 14px;
    overflow: hidden;
}

/* Selectbox */
div[data-baseweb="select"] > div {
    border-radius: 10px;
}

</style>
""", unsafe_allow_html=True)

# -------------------------------------------------
# Session Initialization
# -------------------------------------------------
if 'system' not in st.session_state:
    st.session_state.system = None
    st.session_state.initialized = False

def init_system():
    if not st.session_state.initialized:
        with st.spinner("Initializing system..."):
            st.session_state.system = UniversitySystem("main_database.json")
            st.session_state.system.initialize_system()
            st.session_state.initialized = True
        st.success("System initialized successfully!")

init_system()

# -------------------------------------------------
# Sidebar Navigation
# -------------------------------------------------
st.sidebar.markdown("## 🎓 Smart University")
st.sidebar.caption("Management System")

page = st.sidebar.selectbox(
    "Navigate",
    ["Dashboard", "Add Student", "Top Performer", "At-Risk Students"]
)

# -------------------------------------------------
# DASHBOARD
# -------------------------------------------------
if page == "Dashboard":

    st.markdown("## 📊 Student Dashboard")
    st.caption("Overview of student academic performance")

    system = st.session_state.system
    students = system.students

    if students:

        col1, col2, col3, col4 = st.columns(4)

        with col1:
            st.metric("Total Students", len(students))

        with col2:
            distinction_count = len([s for s in students if s.distinction_status])
            st.metric("Distinction", distinction_count)

        with col3:
            grad_eligible = len([s for s in students if s.graduation_eligibility])
            st.metric("Graduation Eligible", grad_eligible)

        with col4:
            at_risk = len(system.get_at_risk_students())
            st.metric("At-Risk", at_risk)

        st.markdown("### 👥 All Students")

        table_data = []
        for student in students:
            table_data.append({
                "ID": student.id,
                "Name": student.name,
                "Program": student.program,
                "Average": f"{student.average:.2f}" if student.average else "N/A",
                "Distinction": "Yes" if student.distinction_status else "No",
                "Graduation Eligible": "Yes" if student.graduation_eligibility else "No",
                "Performance": student.predict_performance()
            })

        st.dataframe(table_data, use_container_width=True, hide_index=True)

    else:
        st.info("No students found. Add students to begin.")

# -------------------------------------------------
# ADD STUDENT
# -------------------------------------------------
elif page == "Add Student":

    st.markdown("## ➕ Add New Student")
    st.caption("Enter student information below")

    with st.form("add_student_form"):

        col1, col2 = st.columns(2)

        with col1:
            student_id = st.number_input("Student ID *", min_value=1, step=1)
            name = st.text_input("Name *")
            program = st.selectbox("Program *", ["CS", "IT"])

        with col2:
            completed_courses = st.text_input("Completed Courses (comma separated)")
            grades = st.text_input("Grades (Format: CS101:85, CS102:90)")

        submitted = st.form_submit_button("Add Student", use_container_width=True)

        if submitted:
            system = st.session_state.system

            if not student_id or not name:
                st.error("Please fill in required fields.")
            else:
                if any(s.id == int(student_id) for s in system.students):
                    st.error("Student ID already exists.")
                else:
                    try:
                        completed_courses_list = [c.strip() for c in completed_courses.split(',')] if completed_courses else []

                        grades_dict = {}
                        if grades:
                            for pair in grades.split(','):
                                course, grade = pair.split(':')
                                grades_dict[course.strip()] = float(grade.strip())

                        system.add_student_to_database(
                            int(student_id),
                            name,
                            program,
                            completed_courses_list,
                            grades_dict
                        )

                        st.session_state.initialized = False
                        init_system()

                        st.success("Student added successfully!")
                        st.balloons()

                    except Exception as e:
                        st.error(f"Error: {str(e)}")

# -------------------------------------------------
# TOP PERFORMER
# -------------------------------------------------
elif page == "Top Performer":

    st.markdown("## 🏆 Top Performer")

    system = st.session_state.system
    top_student = system.get_top_performer()

    if top_student:
        st.markdown(f"""
        <div style="
            background-color:#d1fae5;
            padding:40px;
            border-radius:25px;
            text-align:center;
            box-shadow:0px 10px 25px rgba(0,0,0,0.06);
        ">
            <h1 style="color:#065f46;">🥇 {top_student.name}</h1>
            <h3 style="color:#047857;">ID: {top_student.id}</h3>
            <h3 style="color:#047857;">Program: {top_student.program}</h3>
            <h1 style="color:#064e3b; font-size:3em;">
                {top_student.average:.2f}
            </h1>
            <p style="color:#065f46;">Average Grade</p>
        </div>
        """, unsafe_allow_html=True)

    else:
        st.info("No top performer found.")

# -------------------------------------------------
# AT-RISK STUDENTS
# -------------------------------------------------
elif page == "At-Risk Students":

    st.markdown("## ⚠️ At-Risk Students")

    system = st.session_state.system
    at_risk_students = system.get_at_risk_students()

    if at_risk_students:

        st.metric("Total At-Risk Students", len(at_risk_students))

        for student in at_risk_students:
            with st.expander(f"{student.name} (ID: {student.id})"):
                st.write(f"Program: {student.program}")
                st.write(f"Average: {student.average}")
                st.write(f"Performance: {student.predict_performance()}")

    else:
        st.success("No at-risk students found.")

# -------------------------------------------------
# Footer
# -------------------------------------------------
st.sidebar.markdown("---")
st.sidebar.caption("Powered by Python • Haskell • Prolog")