
#include <iostream>
#include <fstream>
#include <vector>

using namespace std;

// method to search for every instance of 'a'
void singleChar() {
  string line;
  bool found = false;
  char currentChar;
  vector<int> v;
  int count = 0;


  ifstream myfile;
  myfile.open("test.txt");

  if(myfile.is_open()){

    while( getline (myfile,line)){

      for(int i = 0; i < line.length(); ++i ){
        count++;
        currentChar = line[i];

        if(currentChar == 'a')
            v.push_back(count);
            found = true;

      }

    }


  }
  myfile.close();

  cout << "Instances of 'a' :" << endl;

  cout << "[ ";
  for(int i = 0; i < v.size(); i++) {
    cout << v[i] << " ";
  }
  cout << "]\n" << endl;
}

// method to search for every instanec of 'ab'
void doubleChar() {
  string line;
  bool found = false;
  char currentChar;
  vector<int> v;
  int count = 0;

  ifstream myfile;
  myfile.open("test.txt");

  if(myfile.is_open()) {
    LOOP:
      while ( getline(myfile, line)) {
        int index = 0;
        goto q0;

        q0:
          currentChar = line[index++];
          count++;
          if(index <= line.length()) {
            switch(currentChar) {
              case 'a' : goto q1;
              case 'b' : goto q0;
              default : goto q0;
            }
          }
          else {count--; goto LOOP;}

        q1:

          currentChar = line[index++];
          count++;
          if(index <= line.length()) {
            switch(currentChar) {
              case 'a' : goto q1;
              case 'b' : goto q2;
              default : goto q0;
            }
          }
          else {count--; goto LOOP;}

        q2:
          found = true;
          currentChar = line[index++];
          v.push_back(count);
          count++;
          if(index <= line.length()) {
            switch(currentChar) {
              case 'a' : goto q1;
              case 'b' : goto q0;
              default : goto q0;
            }
          }
          else {count--; goto LOOP;}
      }
  }
  myfile.close();

  cout << "Instances of 'ab' :" << endl;

  cout << "[ ";
  for(int i = 0; i < v.size(); i++) {
    cout << v[i] << " ";
  }
  cout << "]\n" << endl;
}

// method to search for every instance of 'aa'
void doubleSingleChar() {
  string line;
  bool found = false;
  char currentChar;
  vector<int> v;
  int count = 0;

  ifstream myfile;
  myfile.open("test.txt");

  if(myfile.is_open()) {
    LOOP:
      while ( getline(myfile, line)) {
        int index = 0;
        goto q0;

        q0:
          currentChar = line[index++];
          count++;
          if(index <= line.length()) {
            switch(currentChar) {
              case 'a' : goto q1;
              default : goto q0;
            }
          }
          else {count--; goto LOOP;}

        q1:
          currentChar = line[index++];
          //v.push_back(count);
          count++;
          if(index <= line.length()) {
            switch(currentChar) {
              case 'a' : goto q2;
              default : goto q0;
            }
          }
          else {count--; goto LOOP;}

        q2:
          found = true;
          currentChar = line[index++];
          v.push_back(count);
          count++;
          if(index <= line.length()) {
            switch(currentChar) {
              case 'a' : goto q2;
              default : goto q0;
            }
          }
          else {count--; goto LOOP;}
      }
  }
  myfile.close();

  cout << "Instances of 'aa' :" << endl;

  cout << "[ ";
  for(int i = 0; i < v.size(); i++) {
    cout << v[i] << " ";
  }
  cout << "]\n" << endl;
}

// method to search for every instance of 'abc'
void tripleChar() {
  string line;
  bool found = false;
  char currentChar;
  vector<int> v;
  int count = 0;

  ifstream myfile;
  myfile.open("test.txt");

  if(myfile.is_open()) {
    LOOP:
      while ( getline(myfile, line)) {
        int index = 0;
        goto q0;

        q0:
          currentChar = line[index++];
          count++;
          if(index <= line.length()) {
            switch(currentChar) {
              case 'a' : goto q1;
              case 'b' : goto q0;
              case 'c' : goto q0;
              default : goto q0;
            }
          }
          else {count--; goto LOOP;}

        q1:

          currentChar = line[index++];
          count++;
          if(index <= line.length()) {
            switch(currentChar) {
              case 'a' : goto q1;
              case 'b' : goto q2;
              default : goto q0;
            }
          }
          else {count--; goto LOOP;}

        q2:
          currentChar = line[index++];
          count++;
          if(index <= line.length()) {
            switch(currentChar) {
              case 'a' : goto q1;
              case 'b' : goto q0;
              case 'c' : goto q3;
              default : goto q0;
            }
          }
          else {count--; goto LOOP;}

        q3:
          found = true;
          currentChar = line[index++];
          v.push_back(count);
          count++;
          if(index <= line.length()) {
            switch(currentChar) {
              case 'a' : goto q1;
              case 'b' : goto q0;
              case 'c' : goto q0;
              default : goto q0;
            }
          }
          else {count--; goto LOOP;}
      }
  }
  myfile.close();

  cout << "Instances of 'abc' :" << endl;

  cout << "[ ";
  for(int i = 0; i < v.size(); i++) {
    cout << v[i] << " ";
  }
  cout << "]\n" << endl;
}

// method to search for every instance of 'aa*aa'
void wildcard() {
  string line;
  bool end = false;
  bool found = false;
  char currentChar;
  vector<int> v;
  int count = 0;

  ifstream myfile;
  myfile.open("test.txt");

  if(myfile.is_open()) {
    LOOP:
      while ( getline(myfile, line)) {
        int index = 0;
        goto q0;

        q0:
          currentChar = line[index++];
          count++;
          if(index <= line.length()) {
            switch(currentChar) {
              case 'a' : goto q1;
              default : goto q0;
            }
          }
          else {count--; goto LOOP;}

        q1:

          currentChar = line[index++];
          count++;
          if(index <= line.length()) {
            switch(currentChar) {
              case 'a' : goto q2;
              default : goto q0;
            }
          }
          else {count--; goto LOOP;}

        q2:

          if(end) {
            //cout << "Char: " << currentChar << " , Index: " << index-1 << endl;
            found = true;
            currentChar = line[index++];
            //cout << currentChar << endl;

            v.push_back(count);
            count++;
            //end = true;

            if(index <= line.length()) {
              switch(currentChar) {
                case 'a' : goto q2;
                default : goto q0;
              }
            }
            else {count--; goto LOOP;}

          }

          end = true;
          currentChar = line[index++];
          count++;
          if(index <= line.length()) {
            switch(currentChar) {
              case 'a' : goto q1;
              default : goto q0;
            }
          }
          else {count--; goto LOOP;}
      }
  }
  myfile.close();

  cout << "Instances of 'aa*aa' :" << endl;

  cout << "[ ";
  for(int i = 0; i < v.size(); i++) {
    cout << v[i] << " ";
  }
  cout << "]\n" << endl;
}

// method to search for every instance of 'aa' or 'aab'
void doubleSequence() {
  string line;

  bool found = false;
  char currentChar;
  vector<int> v;
  int count = 0;

  ifstream myfile;
  myfile.open("test.txt");

  if(myfile.is_open()) {
    LOOP:
      while ( getline(myfile, line)) {
        int index = 0;
        goto q0;

        q0:
          currentChar = line[index++];
          count++;
          if(index <= line.length()) {
            switch(currentChar) {
              case 'a' : goto q1;
              default : goto q0;
            }
          }
          else {count--; goto LOOP;}

        q1:

          currentChar = line[index++];
          count++;
          if(index <= line.length()) {
            switch(currentChar) {
              case 'a' :
                found = true;
                v.push_back(count);
                goto q2;
              default : goto q0;
            }
          }
          else {count--; goto LOOP;}

        q2:

          currentChar = line[index++];
          count++;
          if(index <= line.length()) {
            switch(currentChar) {
              case 'a' :
                v.push_back(count);
                goto q2;
              case 'b' :
                v.push_back(count);
                goto q0;
              default : goto q0;
            }
          }
          else {count--; goto LOOP;}
      }
  }
  myfile.close();

  cout << "Instances of 'aa' or 'aab' :" << endl;

  cout << "[ ";
  for(int i = 0; i < v.size(); i++) {
    cout << v[i] << " ";
  }
  cout << "]\n" << endl;
}


int main() {
  singleChar();
  doubleChar();
  doubleSingleChar();
  tripleChar();
  wildcard();
  doubleSequence();
}
