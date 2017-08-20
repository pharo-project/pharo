I represent an error produced when asking globals SystemDictionary for a class that doesn't exist.

Example:
Smalltalk globals classNamed: #Number. "=> Number"
Smalltalk  globals classNamed: #notExistedClass. "=> ClassNotFound error"

Object environment classNamed: #Number. "=> Number"
Object environment classNamed: #notExistedClass. "=> ClassNotFound error"