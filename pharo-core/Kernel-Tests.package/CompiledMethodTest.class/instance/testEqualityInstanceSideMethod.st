tests - comparing
testEqualityInstanceSideMethod
   	| method1 method2 |

	method1 :=  TestCase compiler compile: 'aMethod'.
	method2 :=  TestCase compiler compile: 'aMethod'.

	self assert: (method1 literalAt: method1 numLiterals) ==  (method2 literalAt: method2 numLiterals).
 	self assert: method1 = method2.

