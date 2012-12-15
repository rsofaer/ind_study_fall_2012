#include <iostream>
#include <string>


#ifdef MYAPP_EPETRA
#include "Epetra_SerialDenseVector.h"
#endif

#include "Epetra_SerialComm.h"
#include "Epetra_CrsMatrix.h"
int main(int argc, char *argv[]) {
 Epetra_Object::SetTracebackMode(2);
 Epetra_SerialComm Comm;
 char *mtx_filename = argv[1];
 std::cout << mtx_filename << std::endl;
 Epetra_CrsMatrix* mat;
 int res = EpetraExt::MatrixMarketFileToCrsMatrix(mtx_filename, Comm, mat, false, true);
 std::cout << res << ", " << errno << ", " << mat << std::endl;
 //std::cout << mat->NormOne() << std::endl;

}
