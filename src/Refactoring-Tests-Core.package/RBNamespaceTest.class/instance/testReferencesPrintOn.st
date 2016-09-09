class tests
testReferencesPrintOn
	| hasFoundObject hasFoundSelf smalltalk |
	hasFoundObject := false.
	hasFoundSelf := false.
	smalltalk := RBClassModelFactory rbNamespace new.
	smalltalk allReferencesTo: #printOn: do: [ :each | 
		hasFoundObject := hasFoundObject
			or: [ each selector = #fullPrintString 
			and: [each modelClass = (smalltalk classNamed: #Object) ] ].
		hasFoundSelf := hasFoundSelf
			or: [ each selector = #testReferencesPrintOn 
			and: [ each modelClass = (smalltalk classNamed: self class name) ] ] ].
	self assert: hasFoundObject.
	self assert: hasFoundSelf