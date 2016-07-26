search and filtering
doSearchOrFilter
	inputText isEmptyOrNil
		ifTrue: [ self resetInput ]
		ifFalse:[ 
			self glamourPresentation allowsFilter 
				ifTrue: [ self executeFilter ]
				ifFalse: [ self executeSearch ] ].
