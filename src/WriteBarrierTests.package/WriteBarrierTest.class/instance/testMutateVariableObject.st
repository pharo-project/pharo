tests - object
testMutateVariableObject
	| guineaPigs |
	guineaPigs := {#[1 2 3] . #(1 2 3) }.
	guineaPigs do: [ :guineaPig | 
		guineaPig beReadOnlyObject.
		[guineaPig at: 1 put: 4] 
			on: Error 
			do: [ "Surely a NoModification error" ].
		guineaPig
			beWritableObject;
			at: 2 put:  5;
			beReadOnlyObject.
		[guineaPig at: 3 put: 6] 
			on: Error 
			do: [ "Surely a NoModification error" ].
		self assert: guineaPig first = 1.
		self assert: guineaPig second = 5.
		self assert: guineaPig third = 3 ]