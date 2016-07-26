tests - comparing
testEqualityClassSideMethod
   	| method1 method2 |

	method1 := TestCase class compiler
			compile: 'aMethod'.
		
	method2 := TestCase class compiler
			compile: 'aMethod'.
			

 	self assert: method1 = method2.

