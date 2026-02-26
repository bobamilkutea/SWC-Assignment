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
<link rel="stylesheet" href="https://fonts.googleapis.com/css2?family=Material+Symbols+Rounded:opsz,wght,FILL,GRAD@24,400,0,0" />

<style>

/* Main Background */
.stApp {
    background-color: #f4fffb;
    color: #1e293b; /* Ensure text is dark and readable */
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
    background-color: #059669; /* Darker emerald for better contrast with white text */
    color: white;
    border-radius: 12px;
    border: none;
    padding: 0.6rem 1rem;
    font-weight: 500;
}

.stButton>button:hover {
    background-color: #047857; /* Even darker on hover */
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

/* ------------------------------------------------- */
/* NAVIGATION STYLING (Custom Radio Buttons) */
/* ------------------------------------------------- */

/* Hide the default radio circles */
div[role="radiogroup"] > label > div:first-child {
    display: none !important;
}

/* Style the container of the radio options to look like list items */
div[role="radiogroup"] {
    gap: 8px;
    display: flex;
    flex-direction: column;
}

/* Style individual radio labels (the clickable area) */
div[role="radiogroup"] label {
    background-color: transparent;
    padding: 12px 16px;
    border-radius: 10px;
    transition: all 0.2s ease;
    margin-bottom: 4px;
    border: 1px solid transparent;
    cursor: pointer;
    width: 100%;
}

/* Hover effect for navigation items */
div[role="radiogroup"] label:hover {
    background-color: #d1fae5;
    border-color: #a7f3d0;
    transform: translateX(5px);
}

/* Active/Selected state styling */
/* We target the label that contains the checked input using :has() selector */
/* Note: :has() is supported in most modern browsers. Fallback is acceptable. */
div[role="radiogroup"] label:has(input:checked) {
    background-color: #10b981 !important;
    color: white !important;
    font-weight: 600;
    box-shadow: 0 4px 6px -1px rgba(16, 185, 129, 0.3);
}

/* Ensure text inside the active label is white */
div[role="radiogroup"] label:has(input:checked) p {
    color: white !important;
    font-weight: bold;
}

/* General text styling for nav items */
div[role="radiogroup"] label p {
    font-size: 1rem;
    margin: 0;
    display: flex;
    align-items: center;
    gap: 12px;
}

/* ------------------------------------------------- */
/* ICONS INJECTION (Material Symbols) */
/* ------------------------------------------------- */

/* Common style for all icons */
div[role="radiogroup"] label p::before {
    font-family: 'Material Symbols Rounded';
    font-weight: normal;
    font-style: normal;
    font-size: 1.3rem;
    line-height: 1;
    letter-spacing: normal;
    text-transform: none;
    display: inline-block;
    white-space: nowrap;
    word-wrap: normal;
    direction: ltr;
    -webkit-font-feature-settings: 'liga';
    -webkit-font-smoothing: antialiased;
}

/* Dashboard Icon */
div[role="radiogroup"] label:nth-of-type(1) p::before {
    content: "\\e871"; /* dashboard */
}

/* Add Student Icon */
div[role="radiogroup"] label:nth-of-type(2) p::before {
    content: "\\e7fe"; /* person_add */
}

/* Top Performer Icon */
div[role="radiogroup"] label:nth-of-type(3) p::before {
    content: "\\ea38"; /* emoji_events */
}

/* At-Risk Students Icon */
div[role="radiogroup"] label:nth-of-type(4) p::before {
    content: "\\e002"; /* warning */
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

# Custom styled radio button navigation
page_selection = st.sidebar.radio(
    "Navigate",
    ["Dashboard", "Add Student", "Top Performer", "At-Risk Students"],
    label_visibility="collapsed"
)

page = page_selection

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