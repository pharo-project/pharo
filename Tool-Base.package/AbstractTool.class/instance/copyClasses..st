class
copyClasses: aCollection
	
	| needsRefresh lastDuplicate | 
	
	aCollection ifEmpty: [ ^ false -> nil ].
	
	needsRefresh := false.
	aCollection do: [:class || copysName |
		copysName := UIManager default request: 'Please type new class name' initialAnswer: class name.
		copysName isEmptyOrNil
			ifFalse: [
				needsRefresh ifFalse: [ needsRefresh := true ].
				lastDuplicate := class duplicateClassWithNewName: copysName ]].

	^(needsRefresh -> lastDuplicate)