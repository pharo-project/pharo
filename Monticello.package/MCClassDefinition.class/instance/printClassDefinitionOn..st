printing
printClassDefinitionOn: stream
	"Print a class-side definition of the receiver on the given stream.
	Class instance variables and class traits."
	
		stream
			nextPutAll: self className;
			nextPutAll: ' class';
			cr; tab.
		self hasClassTraitComposition ifTrue: [
			stream 
				nextPutAll: 'uses: ';
		 		nextPutAll: self classTraitCompositionString;
				cr; tab ].
		stream
			nextPutAll: 'instanceVariableNames: ';
			store: self classInstanceVariablesString