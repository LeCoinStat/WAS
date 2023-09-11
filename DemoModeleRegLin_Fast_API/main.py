from fastapi import FastAPI
from pydantic import BaseModel
from sklearn.linear_model import LinearRegression
import numpy as np
import statsmodels.api as sm
import shap

app = FastAPI()

# Génération de données fictives initiales
np.random.seed(0)
taille_pere_init = np.random.uniform(160, 190, 100)
taille_mere_init = np.random.uniform(150, 180, 100)
bruit = np.random.normal(0, 5, 100)
taille_enfant_init = 0.5 * taille_pere_init + 0.4 * taille_mere_init + 30 + bruit

data_global = {
    "taille_pere": taille_pere_init.tolist(),
    "taille_mere": taille_mere_init.tolist(),
    "taille_enfant": taille_enfant_init.tolist()
}

modele = LinearRegression()
modele.fit(np.column_stack((taille_pere_init, taille_mere_init)), taille_enfant_init)

X_sm = sm.add_constant(np.column_stack((taille_pere_init, taille_mere_init)))
modele_sm = sm.OLS(taille_enfant_init, X_sm).fit()

expliqueur = shap.Explainer(modele, shap.sample(np.column_stack((taille_pere_init, taille_mere_init)), 100))

class EntreePrediction(BaseModel):
    taille_pere: float
    taille_mere: float

class NouvellesDonnees(BaseModel):
    taille_pere: list[float]
    taille_mere: list[float]
    taille_enfant: list[float]

@app.get("/")
def accueil():
    return {"message": "Bienvenue sur notre API de prédiction de la taille d'un enfant !"}

@app.post("/predire/")
def predire(donnees: EntreePrediction):
    x = np.array([donnees.taille_pere, donnees.taille_mere]).reshape(1, -1)
    prediction = modele.predict(x)[0]
    valeurs_shap = expliqueur(x)
    
    return {
        "prediction": prediction,
        "valeurs_shap": valeurs_shap.values[0].tolist()
    }

@app.get("/parametres/")
def obtenir_parametres():
    noms_variables = ["Intercept", "taille_pere", "taille_mere"]
    coeffs = modele.coef_.tolist()
    coeffs.insert(0, modele.intercept_)
    
    r2 = modele_sm.rsquared
    p_valeurs = modele_sm.pvalues.tolist()
    significativite = ["Significatif" if p < 0.05 else "Non significatif" for p in modele_sm.pvalues]
    
    return {
        "noms_variables": noms_variables,
        "coefficients": coeffs,
        "r2": r2,
        "p_valeurs": p_valeurs,
        "significativite": significativite
    }

@app.post("/ajouter_donnees/")
def ajouter_donnees(data: NouvellesDonnees):
    global modele, modele_sm

    # Mise à jour des données
    data_global["taille_pere"].extend(data.taille_pere)
    data_global["taille_mere"].extend(data.taille_mere)
    data_global["taille_enfant"].extend(data.taille_enfant)

    X = np.column_stack((data_global["taille_pere"], data_global["taille_mere"]))
    y = np.array(data_global["taille_enfant"])

    # Entraîner un nouveau modèle
    modele = LinearRegression().fit(X, y)

    X_sm = sm.add_constant(X)
    modele_sm = sm.OLS(y, X_sm).fit()
    
    # Obtenir les nouveaux paramètres du modèle
    noms_variables = ["Intercept", "taille_pere", "taille_mere"]
    coeffs = modele.coef_.tolist()
    coeffs.insert(0, modele.intercept_)
    
    r2 = modele_sm.rsquared
    p_valeurs = modele_sm.pvalues.tolist()
    significativite = ["Significatif" if p < 0.05 else "Non significatif" for p in modele_sm.pvalues]

    return {
        "message": "Données ajoutées et modèle mis à jour",
        "nouveaux_parametres": {
            "noms_variables": noms_variables,
            "coefficients": coeffs,
            "r2": r2,
            "p_valeurs": p_valeurs,
            "significativite": significativite
        }
    }
