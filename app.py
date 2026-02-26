import streamlit as st
import json
from python_main import UniversitySystem
from ui.styles import inject_styles
from ui.dashboard import render_dashboard
from ui.add_student import render_add_student
from ui.top_performer import render_top_performer
from ui.at_risk import render_at_risk

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
# Inject Modern CSS Theme
# -------------------------------------------------
inject_styles()

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

# -------------------------------------------------
# Module Router
# -------------------------------------------------
system = st.session_state.system

if page_selection == "Dashboard":
    render_dashboard(system)

elif page_selection == "Add Student":
    render_add_student(system)

elif page_selection == "Top Performer":
    render_top_performer(system)

elif page_selection == "At-Risk Students":
    render_at_risk(system)

# -------------------------------------------------
# Footer
# -------------------------------------------------
st.sidebar.markdown("---")
st.sidebar.caption("Powered by Python • Haskell • Prolog")
