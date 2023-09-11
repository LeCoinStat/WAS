from fastapi import FastAPI
import random

app = FastAPI()

@app.get("/")
def accueil():
    return {"message": "Bienvenue sur notre API de génération de nombres aléatoires!"}

@app.get("/nombre_aleatoire/")
def generer_nombre():
    """
    Génère un nombre aléatoire entre 0 et 100.
    """
    return {"nombre_aleatoire": random.randint(0, 100)}

@app.get("/nombre_aleatoire/{min}_{max}")
def generer_nombre(min: int = 0, max: int = 100):
    """
    Génère un nombre aléatoire entre min et max.
    """
    return {"nombre_aleatoire": random.randint(min, max)}
# uvicorn main:app --reload