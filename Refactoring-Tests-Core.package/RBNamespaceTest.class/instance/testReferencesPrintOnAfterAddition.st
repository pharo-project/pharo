class tests
testReferencesPrintOnAfterAddition
	| hasFoundObject hasFoundSelf smalltalk |
	hasFoundObject := false.
	hasFoundSelf := false.
	smalltalk := RBNamespace new.
	(smalltalk classNamed: #Object) 
		compile: 'someTestReference self printOn: nil'
		classified: #(testing).
	(smalltalk classNamed: self class name)
		compile: 'someTestReference ^ #printOn:'
		classified: #(testing).
	smalltalk allReferencesTo: #printOn: do: [ :each | 
		hasFoundObject := hasFoundObject 
			or: [ each selector = #someTestReference 
			and: [ each modelClass = (smalltalk classNamed: #Object) ] ].
		hasFoundSelf := hasFoundSelf
			or: [ each selector = #someTestReference 
			and: [ each modelClass = (smalltalk classNamed: self class name) ] ] ].
	self assert: hasFoundObject.
	self assert: hasFoundSelf