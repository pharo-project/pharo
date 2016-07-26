class tests
testReferencesPrintOnAfterRemove
	| hasFoundObject hasFoundSelf smalltalk |
	hasFoundObject := false.
	hasFoundSelf := false.
	smalltalk := RBNamespace new.
	(smalltalk classNamed: #Object) 
		removeMethod: #printString.
	(smalltalk classNamed: self class name) 
		removeMethod: #testReferencesPrintOnAfterRemove.
	smalltalk allReferencesTo: #printOn: do: [ :each | 
		hasFoundObject := hasFoundObject
			or: [ each selector = #printString 
			and: [ each modelClass = (smalltalk classNamed: #Object) ] ].
		hasFoundSelf := hasFoundSelf
			or: [ each selector = #testReferencesPrintOnAfterRemove 
			and: [ each modelClass = (smalltalk classNamed: self class name) ] ] ].
	self deny: hasFoundObject.
	self deny: hasFoundSelf