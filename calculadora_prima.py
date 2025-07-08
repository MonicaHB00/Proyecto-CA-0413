import streamlit as st
import pandas as pd
import math

# Configuración de la página con theme
st.set_page_config(
    page_title="Calculadora de Prima de Riesgo GLM",
    layout="wide",
    initial_sidebar_state="expanded"
)

# --------------------------- TEMA STREAMLIT ---------------------------
# Puedes copiar este bloque en .streamlit/config.toml para que sea global
st.write('''
<style>
    /* El tema se gestiona desde config.toml, pero puedes personalizar aquí si lo deseas */
</style>
''', unsafe_allow_html=True)

# --------------------------- COEFICIENTES ---------------------------
# Bernoulli
INTERCEPTO_BERNOULLI = -2.99656
COEF_BER_VEH_BODY = {
    'BUS': 0, 'CONVT': -1.65227, 'COUPE': -0.64885, 'HBACK': -1.10942,
    'HDTOP': -0.91006, 'MCARA': -0.41673, 'MIBUS': -1.09628, 'PANVN': -1.00071,
    'RDSTR': -0.99297, 'SEDAN': -1.07585, 'STNWG': -1.01342, 'TRUCK': -1.1105, 'UTE': -1.26538
}
COEF_BER_VEH_AGE = {1: 0, 2: 0.05161, 3: -0.07312, 4: -0.16839}
COEF_BER_AGECAT = {1: 0, 2: -0.20381, 3: -0.25744, 4: -0.29333, 5: -0.50842, 6: -0.51218}
COEF_BER_SQRT_EXPOSURE = 2.56662
EXPOSURE_T = {
    '1 Hora': 0.000114077,
    '8 Horas': 0.00091262,
    '1 Día': 0.002737851,
    '1 Semana': 0.019230769,
    '1 Mes': 0.08333333,
    '1 Año': 1.0,
    'Personal': 0.9993155
}
E_X = 2014.40
E_X2 = 16649837.72
V_X = 12592013.94

# Poisson
INTERCEPTO_POISSON = -3.467808595
COEF_POISSON = {
    'veh_body': {
        'BUS': 0, 'CONVT': -1.45832122, 'COUPE': -0.54032285, 'HBACK': -0.95336773,
        'HDTOP': -0.75978828, 'MCARA': -0.31236742, 'MIBUS': -0.94381659, 'PANVN': -0.82716151,
        'RDSTR': -0.8367931, 'SEDAN': -0.92104257, 'STNWG': -0.85568926, 'TRUCK': -0.9173654, 'UTE': -1.07901597
    },
    'veh_age': {1: 0, 2: 0.047293564, 3: -0.065250404, 4: -0.151682699},
    'agecat': {1: 0, 2: -0.184940293, 3: -0.231580488, 4: -0.263179122, 5: -0.457128651, 6: -0.452787786},
    'area': {'A': 0, 'B': 0.082504626, 'C': 0.033263404, 'D': -0.08064239, 'E': -0.014896287, 'F': 0.072815003},
    'gender': {'F': 0, 'M': -0.014574857},
    'exposure_terms': {
        'I(exposure)': 0.498767859,
        'I(exposure^2)': -0.784363978,
        'I(sqrt(exposure))': 2.852200666
    }
}

# Cargas fijas
CARGAS = {
    "Adquisición": 0.10,
    "Gastos": 0.05,
    "Bonificación e Incertidumbre": 0.05,
    "Utilidad": 0.20
}
TOTAL_CARGAS = sum(CARGAS.values())

# --------------------------- FUNCIONES COMUNES ---------------------------
def sigmoid(x):
    return 1 / (1 + math.exp(-x))

def calcular_pi_xb(xb):
    return sigmoid(xb)

def calcular_mu_poisson(eta):
    return math.exp(eta)

def calcular_prima_riesgo_daños(p, E_X, E_X2, k):
    return p * E_X + k * math.sqrt(p * E_X2)

def calcular_prima_riesgo_perdida(p, sa, k):
    return sa * (p + k * math.sqrt(p * (1 - p)))

def calcular_prima_tarifa(prima_riesgo):
    return prima_riesgo / (1 - TOTAL_CARGAS)

# --------------------------- INTERFAZ ---------------------------
def main():
    st.title(" Calculadora de Prima de Riesgo GLM")
    st.write("---")
    tab1, tab2 = st.tabs(["Modelo Bernoulli", "Modelo Poisson"])

    # ----------------------- BERNOUILLI ----------------------------
    with tab1:
        st.header(" Modelo Bernoulli")
        veh_body = st.selectbox("Carrocería:", list(COEF_BER_VEH_BODY.keys()), key="ber_body")
        veh_age = st.selectbox("Edad Vehículo:", list(COEF_BER_VEH_AGE.keys()), key="ber_age")
        agecat = st.selectbox("Categoría de Edad:", list(COEF_BER_AGECAT.keys()), key="ber_agecat")
        exposure = st.selectbox("Exposición:", list(EXPOSURE_T.keys()), key="ber_expo")

        sqrt_exp = math.sqrt(EXPOSURE_T[exposure])
        xb = INTERCEPTO_BERNOULLI + COEF_BER_VEH_BODY[veh_body] + COEF_BER_VEH_AGE[veh_age] + COEF_BER_AGECAT[agecat] + COEF_BER_SQRT_EXPOSURE * sqrt_exp
        p = calcular_pi_xb(xb)

        st.write(f"**Xβ:** {xb:.6f} | **P(Xβ):** {p*100:.2f}%")

        cobertura = st.radio("Cobertura:", ["Daños Materiales", "Pérdida Total"], key="ber_cobertura")
        k = st.number_input("Parámetro k:", min_value=0.0, max_value=1.0, value=0.05, step=0.01, key="ber_k")

        if cobertura == "Pérdida Total":
            sa = st.number_input("Suma Asegurada:", min_value=0.0, value=5000.0, step=100.0, key="ber_sa")

        if cobertura == "Daños Materiales":
            prima_riesgo = calcular_prima_riesgo_daños(p, E_X, E_X2, k)
        else:
            prima_riesgo = calcular_prima_riesgo_perdida(p, sa, k)

        prima_tarifa = calcular_prima_tarifa(prima_riesgo)

        st.success(f"Prima de Riesgo: ${prima_riesgo:,.2f}")
        st.success(f"Prima de Tarifa: ${prima_tarifa:,.2f}")

    # ----------------------- POISSON ----------------------------
    with tab2:
        st.header(" Modelo Poisson")
        veh_body = st.selectbox("Carrocería:", list(COEF_POISSON['veh_body'].keys()), key="pois_body")
        veh_age = st.selectbox("Edad Vehículo:", list(COEF_POISSON['veh_age'].keys()), key="pois_age")
        agecat = st.selectbox("Categoría de Edad:", list(COEF_POISSON['agecat'].keys()), key="pois_agecat")
        gender = st.selectbox("Género:", list(COEF_POISSON['gender'].keys()), key="pois_gender")
        area = st.selectbox("Área:", list(COEF_POISSON['area'].keys()), key="pois_area")
        exposure = st.selectbox("Exposición:", list(EXPOSURE_T.keys()), key="pois_expo")

        t = EXPOSURE_T[exposure]
        eta = INTERCEPTO_POISSON \
            + COEF_POISSON['veh_body'][veh_body] + COEF_POISSON['veh_age'][veh_age] \
            + COEF_POISSON['agecat'][agecat] + COEF_POISSON['gender'][gender] + COEF_POISSON['area'][area] \
            + COEF_POISSON['exposure_terms']['I(exposure)'] * t \
            + COEF_POISSON['exposure_terms']['I(exposure^2)'] * (t ** 2) \
            + COEF_POISSON['exposure_terms']['I(sqrt(exposure))'] * math.sqrt(t)

        mu = calcular_mu_poisson(eta)

        st.write(f"**η (eta):** {eta:.6f} | **μ (mu):** {mu:.6f}")

        cobertura = st.radio("Cobertura:", ["Daños Materiales", "Pérdida Total"], key="pois_cobertura")
        k = st.number_input("Parámetro k:", min_value=0.0, max_value=1.0, value=0.05, step=0.01, key="pois_k")

        if cobertura == "Pérdida Total":
            sa = st.number_input("Suma Asegurada:", min_value=0.0, value=5000.0, step=100.0, key="pois_sa")

        if cobertura == "Daños Materiales":
            prima_riesgo = calcular_prima_riesgo_daños(mu, E_X, E_X2, k)
        else:
            prima_riesgo = calcular_prima_riesgo_perdida(mu, sa, k)

        prima_tarifa = calcular_prima_tarifa(prima_riesgo)

        st.success(f"Prima de Riesgo: ${prima_riesgo:,.2f}")
        st.success(f"Prima de Tarifa: ${prima_tarifa:,.2f}")

if __name__ == "__main__":
    main()
