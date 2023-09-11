from flask import Flask, jsonify
import random

app = Flask(__name__)

@app.route('/')
def accueil():
    return jsonify({"message": "Bienvenue sur notre API de génération de nombres aléatoires!"})

@app.route('/nombre_aleatoire/')
def generer_nombre():
    """ Génère un nombre aléatoire entre 0 et 100. """
    return jsonify({"nombre_aleatoire": random.randint(0, 100)})

@app.route('/nombre_aleatoire/<int:min>_<int:max>')
def generer_nombre_min_max(min, max):
    """ Génère un nombre aléatoire entre min et max. """
    return jsonify({"nombre_aleatoire": random.randint(min, max)})

#if __name__ == '__main__':
app.run(debug=True)
