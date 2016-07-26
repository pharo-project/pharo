menu - method
copyMethodFrom: src to: dest
	"Place this change in the other changeSet also"	
	| other |
	
	self checkThatSidesDiffer: [^ self].
	other := dest selectedChangeSet.
	
	src selectedSelector ifNotNil: [:selector || class |
		class := src selectedClass.
		
		self model 
			copySelector: selector 
			inClass: class 
			from: src selectedChangeSet 
			to: other.
			
		dest updateClassesListAndMessagesList ].