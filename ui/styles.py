import streamlit as st

def inject_styles():
    st.markdown("""
<link rel="stylesheet" href="https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&display=swap" />
<link rel="stylesheet" href="https://fonts.googleapis.com/css2?family=Material+Symbols+Rounded:opsz,wght,FILL,GRAD@24,400,0,0" />

<style>
/* ------------------------------------------------- */
/* 🌿 MODERN THEME VARIABLES */
/* ------------------------------------------------- */
:root {
    --primary: #10b981;
    --primary-dark: #059669;
    --secondary: #d1fae5;
    --bg-main: #f4fffb;
    --bg-card: #ffffff;
    --text-primary: #1e293b;
    --text-secondary: #64748b;
    --border-radius: 12px;
    --shadow-sm: 0 1px 2px 0 rgb(0 0 0 / 0.05);
    --shadow-md: 0 4px 6px -1px rgb(0 0 0 / 0.1), 0 2px 4px -2px rgb(0 0 0 / 0.1);
}

/* ------------------------------------------------- */
/* GLOBAL RESET & TYPOGRAPHY */
/* ------------------------------------------------- */
.stApp {
    background-color: var(--bg-main);
    color: var(--text-primary);
    font-family: 'Inter', sans-serif;
}

h1, h2, h3, h4, h5, h6 {
    color: var(--text-primary);
    font-weight: 600;
    letter-spacing: -0.025em;
}

/* ------------------------------------------------- */
/* SIDEBAR STYLING */
/* ------------------------------------------------- */
section[data-testid="stSidebar"] {
    background-color: #ecfdf5;
    border-right: 1px solid #d1fae5;
}

/* ------------------------------------------------- */
/* NAVIGATION STYLING (Custom Radio Buttons) */
/* ------------------------------------------------- */
div[role="radiogroup"] > label > div:first-child {
    display: none !important;
}

div[role="radiogroup"] {
    gap: 8px;
    display: flex;
    flex-direction: column;
}

div[role="radiogroup"] label {
    background-color: transparent;
    padding: 12px 16px;
    border-radius: var(--border-radius);
    transition: all 0.2s cubic-bezier(0.4, 0, 0.2, 1);
    margin-bottom: 4px;
    border: 1px solid transparent;
    cursor: pointer;
    width: 100%;
}

div[role="radiogroup"] label:hover {
    background-color: var(--secondary);
    border-color: #a7f3d0;
    transform: translateX(4px);
}

div[role="radiogroup"] label:has(input:checked) {
    background-color: var(--primary) !important;
    color: white !important;
    font-weight: 600;
    box-shadow: var(--shadow-md);
}

div[role="radiogroup"] label:has(input:checked) p {
    color: white !important;
    font-weight: 600;
}

div[role="radiogroup"] label p {
    font-size: 0.95rem;
    margin: 0;
    display: flex;
    align-items: center;
    gap: 12px;
    color: var(--text-primary);
}

/* ------------------------------------------------- */
/* ICONS INJECTION (Material Symbols) */
/* ------------------------------------------------- */
div[role="radiogroup"] label p::before {
    font-family: 'Material Symbols Rounded';
    font-weight: normal;
    font-style: normal;
    font-size: 1.25rem;
    line-height: 1;
    display: inline-block;
    white-space: nowrap;
    word-wrap: normal;
    direction: ltr;
    -webkit-font-feature-settings: 'liga';
    -webkit-font-smoothing: antialiased;
}

/* Dashboard Icon */
div[role="radiogroup"] label:nth-of-type(1) p::before { content: "\\e871"; } /* dashboard */
/* Add Student Icon */
div[role="radiogroup"] label:nth-of-type(2) p::before { content: "\\e7fe"; } /* person_add */
/* Top Performer Icon */
div[role="radiogroup"] label:nth-of-type(3) p::before { content: "\\ea38"; } /* emoji_events */
/* At-Risk Students Icon */
div[role="radiogroup"] label:nth-of-type(4) p::before { content: "\\e002"; } /* warning */

/* ------------------------------------------------- */
/* COMPONENT STYLING */
/* ------------------------------------------------- */

/* Metric Cards */
[data-testid="metric-container"] {
    background-color: var(--bg-card);
    border-radius: var(--border-radius);
    padding: 24px;
    box-shadow: var(--shadow-sm);
    border: 1px solid #e2e8f0;
    transition: transform 0.2s ease, box-shadow 0.2s ease;
}

[data-testid="metric-container"]:hover {
    transform: translateY(-2px);
    box-shadow: var(--shadow-md);
}

/* Buttons */
.stButton > button {
    background-color: var(--primary);
    color: white;
    border-radius: var(--border-radius);
    border: none;
    padding: 0.75rem 1.5rem;
    font-weight: 500;
    font-size: 0.95rem;
    transition: background-color 0.2s ease;
    width: 100%;
}

.stButton > button:hover {
    background-color: var(--primary-dark);
    box-shadow: var(--shadow-sm);
}

/* Forms & Inputs */
div[data-testid="stForm"] {
    background-color: var(--bg-card);
    padding: 32px;
    border-radius: var(--border-radius);
    box-shadow: var(--shadow-md);
    border: 1px solid #e2e8f0;
}

input, select, textarea {
    border-radius: 8px !important;
}

/* Dataframe */
[data-testid="stDataFrame"] {
    border-radius: var(--border-radius);
    overflow: hidden;
    box-shadow: var(--shadow-sm);
    border: 1px solid #e2e8f0;
}

/* Expanders */
.streamlit-expanderHeader {
    background-color: var(--bg-card);
    border-radius: var(--border-radius);
    padding: 1rem;
    font-weight: 500;
}

/* Custom Card Styling (for Top Performer) */
.custom-card {
    background: linear-gradient(135deg, #d1fae5 0%, #ffffff 100%);
    border-radius: 24px;
    padding: 48px;
    text-align: center;
    box-shadow: var(--shadow-md);
    border: 1px solid #d1fae5;
    max-width: 600px;
    margin: 0 auto;
}

</style>
""", unsafe_allow_html=True)
