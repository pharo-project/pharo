Class {
	#name : #NewMethodFinder,
	#superclass : #Object,
	#category : #NewMethodFinder
}

{ #category : #'as yet unclassified' }
NewMethodFinder >> guess: inputs and: outputs [
	"Search for methods which giving each elements of input returns the corrsponding 
	 elements of outpts."

	| res answers methods|
	res := OrderedCollection new.
	answers := Dictionary new.
	
	inputs isEmpty ifTrue: [^'Error, no inputs to check.'].
	inputs size = outputs size ifFalse: [^'Error, should have as many input as output.'].
	
	methods := inputs first class approvedMethods.
	inputs do: [ :each |
		methods := methods intersection: (each class approvedMethods)
	]. "Not necesary to get the intersection of approvedMethods."
	
	"Creates dictionnary inputs -> outputs."
	inputs doWithIndex: [ :each :index | 
		answers at: (each) put: (outputs at: index).	
	].

   "Select corresponding methods."
	res := methods select: [ :method | 
		(inputs allSatisfy: [ :input |
			(input perform: method) = (answers at: input)
		])
	].
	^res. 
]
