accessing
comment
	^ comment = LookupComment
		ifTrue: [
			comment := self isDefined
				ifTrue: [ self realClass comment ]
				ifFalse: [ nil ] ]
		ifFalse: [ comment ]