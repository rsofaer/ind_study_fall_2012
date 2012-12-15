#include <iostream>
#include <string>

#ifdef MYAPP_MPI
#include "Teuchos_DefaultSerialComm.hpp"
#endif

#ifdef MYAPP_EPETRA
#include "Epetra_SerialDenseVector.h"
#endif



int main(int argc, char *argv[]) {
 std::cout << "it compiles" << std::endl;
}
