private
retrieveCheckIcon
	^ self selected
		ifTrue: [ Smalltalk ui icons iconNamed: #checkedBoxIcon ]
		ifFalse: [ self isPartialMatch
				ifTrue: [ Smalltalk ui icons iconNamed: #partialCheckedBoxIcon ]
				ifFalse: [ Smalltalk ui icons iconNamed: #uncheckedBoxIcon ] ]