accessing
classPool
	"Answer the dictionary of class variables."

	classPool ifNil: [ classPool := Dictionary new ].
		
	^ classPool