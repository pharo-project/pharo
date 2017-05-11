tests
testClassWithUnderscore

	| cls data reader |

	cls := Object newAnonymousSubclass.
	cls name: #A_B_C123AnonClass.

	data := STON toString: cls new.
	reader := STONReader on: data readStream.

	(reader instVarNamed: #classes) 
		at: cls name 
		put: cls.

	self assert: reader next class equals: cls