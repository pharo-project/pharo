running
newClassWithInstanceVariableNames: instanceVariableNames superclass: aSuperclass
	"Returns a class for testing, with the specified instance variables."

	^ self 
		newSubclassOf: aSuperclass
		instanceVariableNames: instanceVariableNames
		classVariableNames: ''