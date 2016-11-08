private
retrieveCheckIcon
	^ self selected
		ifTrue: [ self iconNamed: #checkedBoxIcon ]
		ifFalse: [ self isPartialMatch
				ifTrue: [ self iconNamed: #partialCheckedBoxIcon ]
				ifFalse: [ self iconNamed: #uncheckedBoxIcon ] ]