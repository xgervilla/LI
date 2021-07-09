#include <iostream>
#include <stdlib.h>
#include <algorithm>
#include <vector>
using namespace std;

#define UNDEF -1
#define TRUE 1
#define FALSE 0

uint numVars;
uint numClauses;
vector<vector<int> > clauses; //literales que aparecen en cada clausula
vector<int> model; //solucion
vector<int> modelStack; //pila de decisiones
uint indexOfNextLitToPropagate;
uint decisionLevel;

vector<vector<int> > occurPositive;
vector<vector<int> > occurNegative; 
vector<int> contPositive;
vector<int> contNegative;

/*
anadimos dos tipos de estructuras:
->occur: permite ver las clausulas en las que aparece el literal
->cont: permite mantener un contador de las veces que aparece el literal para poder tener prioridad a la hora de tomar la siguiente decision

cuando una decision crea conflicto se aumenta en uno el contador de ese literal para tenerlo como mas prioritario a la hora de decidir

posible mejora: contador de literales de conflictos -> cuantas veces aparece el literal en un conflicto o aumentar la penalizacion
//interesa ver los que aparecen más en conflictos

//posible mejora de la mejora: se tiene en cuenta cuando ha aparecido: cada N conflictos se divide el contador entre 2 (permite que las recientes tengan mas valor)
*/

//literales: 1, 2..
//literales negados: -1, -2..
//propagar literales: si propagamos 1 consultamos la occurList de -1 --> occurPositive[y] -> occurNegative[-y]

void readClauses( ){
  // Skip comments
  char c = cin.get();
  while (c == 'c') {
    while (c != '\n') c = cin.get();
    c = cin.get();
  }  
  // Read "cnf numVars numClauses"
  string aux;
  cin >> aux >> numVars >> numClauses;
  clauses.resize(numClauses);

  //asignamos el size de las estructuras (numVars+1 para no usar la posicion 0)
  //aparicion positiva (lit, >0) y negativa/negada (-lit, <0)
  occurPositive.resize(numVars+1);
  occurNegative.resize(numVars+1);
  
  //cantidad de apariciones de cada literal (separando por casos)
  contPositive.resize(numVars+1);
  contNegative.resize(numVars+1);

  // Read clauses
  for (uint i = 0; i < numClauses; ++i) {
    int lit;
    while (cin >> lit and lit != 0){
      clauses[i].push_back(lit); //en la clausula i aparece el literal lit
      if(lit>0){ //positive literal
        //indicamos que aparece como positivo es en la clausula
        occurPositive[lit].push_back(i);
        //aumentamos el numero de
        ++contPositive[lit];
      }
      else{
        occurNegative[-lit].push_back(i);
        ++contNegative[-lit];
      }
    }
  }    
}


int currentValueInModel(int lit){
  if (lit >= 0) return model[lit];
  else {
    if (model[-lit] == UNDEF) return UNDEF;
    else return 1 - model[-lit];
  }
}

void setLiteralToTrue(int lit){
  modelStack.push_back(lit);
  if (lit > 0) model[lit] = TRUE;
  else model[-lit] = FALSE;   
}


bool propagateGivesConflict ( ) {
  while ( indexOfNextLitToPropagate < modelStack.size() ) {
    //analizamos el siguiente literal a propagar
    int litToPropagate = modelStack[indexOfNextLitToPropagate];
    if(litToPropagate>0){
      //si es un literal positivo: propagamos en los negativos -> todas las apariencias de clausulas negativas
      for(uint i = 0; i<occurNegative[litToPropagate].size();++i){
        bool someLitTrue = false;
        int numUndefs = 0;
        int lastLitUndef = 0;
        //cogemos el elemento i de la clausula donde aparece para ver si afecta la decision o si ya estaba decidido (someLitTrue)
        int litClause = occurNegative[litToPropagate][i];
        for(uint k = 0; not someLitTrue and k < clauses[litClause].size(); ++k){
          int val = currentValueInModel(clauses[litClause][k]);
          if(val == TRUE) someLitTrue = true; //si hay alguno que ya esta en TRUE dejamos de buscar (se ha modificado previamente)
          else if(val == UNDEF){
            ++numUndefs;
            lastLitUndef = clauses[litClause][k];
          }
        }
        //al acabar de ver si hay alguno ya modificado comprobamos si hemos provocado conflicto-> si hay conflicto anadimos penalizacion
        if (not someLitTrue and numUndefs == 0){
          ++contPositive[litToPropagate];
          return true; //CONFLICTO
        }
        else if (not someLitTrue and numUndefs == 1) setLiteralToTrue(lastLitUndef);  
      }
    }
    else{
      //si es un literal negativo: propagamos en los positivos
      for(uint i = 0; i<occurPositive[-litToPropagate].size();++i){
        bool someLitTrue = false;
        int numUndefs = 0;
        int lastLitUndef = 0;
        //cogemos el elemento i de la clausula donde aparece para ver si afecta la decision o si ya estaba decidido (someLitTrue)
        int litClause = occurPositive[-litToPropagate][i];
        for(uint k = 0; not someLitTrue and k < clauses[litClause].size(); ++k){
          int val = currentValueInModel(clauses[litClause][k]);
          if(val == TRUE) someLitTrue = true; //si hay alguno que ya esta en TRUE dejamos de buscar (se ha modificado previamente)
          else if(val == UNDEF){
            ++numUndefs;
            lastLitUndef = clauses[litClause][k];
          }
        }
        //al acabar de ver si hay alguno ya modificado comprobamos si hemos provocado conflicto-> si hay conflicto anadimos penalizacion
        if (not someLitTrue and numUndefs == 0){
          ++contNegative[-litToPropagate];
          return true; //CONFLICTO
        }
        else if (not someLitTrue and numUndefs == 1) setLiteralToTrue(lastLitUndef);  
      }
    }
    ++indexOfNextLitToPropagate;    
  }
  return false;
}


void backtrack(){
  uint i = modelStack.size() -1;
  int lit = 0;
  while (modelStack[i] != 0){ // 0 is the DL mark
    lit = modelStack[i];
    model[abs(lit)] = UNDEF;
    modelStack.pop_back();
    --i;
  }
  // at this point, lit is the last decision
  modelStack.pop_back(); // remove the DL mark
  --decisionLevel;
  indexOfNextLitToPropagate = modelStack.size();
  setLiteralToTrue(-lit);  // reverse last decision
}


/*
en vez de devolver el primero indefinido devolvemos el de mayor peso: total de apariciones contPositive+contNegative -> return del max(contPositive,contNegative) 
*/
int getNextDecisionLiteral(){
  int litToReturn = 0; //auxiliar para acumular el literal hasta encontrar el mejor candidato
  int contOccur = -1; //contador de apariciones
  for (uint i = 1; i <= numVars; ++i){
    int suma = contNegative[i]+contPositive[i];
    if(suma>contOccur && model[i] == UNDEF){
      //si la suma es mayor al actual y el literal no esta definido en el modelo
      contOccur = suma;
      if(contNegative[i]>contPositive[i]) litToReturn = -i;
      else litToReturn = i;
    }
  }
  return litToReturn; // reurns 0 when all literals are defined
}

void checkmodel(){
  for (uint i = 0; i < numClauses; ++i){
    bool someTrue = false;
    for (uint j = 0; not someTrue and j < clauses[i].size(); ++j)
      someTrue = (currentValueInModel(clauses[i][j]) == TRUE);
    if (not someTrue) {
      cout << "Error in model, clause is not satisfied:";
      for (uint j = 0; j < clauses[i].size(); ++j) cout << clauses[i][j] << " ";
      cout << endl;
      exit(1);
    }
  }  
}

int main(){ 
  readClauses(); // reads numVars, numClauses and clauses
  model.resize(numVars+1,UNDEF);
  indexOfNextLitToPropagate = 0;  
  decisionLevel = 0;
  
  // Take care of initial unit clauses, if any
  for (uint i = 0; i < numClauses; ++i)
    if (clauses[i].size() == 1) {
      int lit = clauses[i][0];
      int val = currentValueInModel(lit);
      if (val == FALSE) {cout << "UNSATISFIABLE" << endl; return 10;}
      else if (val == UNDEF) setLiteralToTrue(lit);
    }
  
  // DPLL algorithm
  while (true) {
    while ( propagateGivesConflict() ) {
      if ( decisionLevel == 0) { cout << "UNSATISFIABLE" << endl; return 10; }
      backtrack();
    }
    int decisionLit = getNextDecisionLiteral();
    if (decisionLit == 0) { checkmodel(); cout << "SATISFIABLE" << endl; return 20; }
    // start new decision level:
    modelStack.push_back(0);  // push mark indicating new DL
    ++indexOfNextLitToPropagate;
    ++decisionLevel;
    setLiteralToTrue(decisionLit);    // now push decisionLit on top of the mark
  }
}  
