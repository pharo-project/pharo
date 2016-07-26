accessing
message
	^ (includeSelectors and: [ narrowString isEmpty ]) 
		ifTrue: 
			[ selectors size = 500 
				ifTrue: [ 'more...' ]
				ifFalse: [ 'press key for selectors' ] ]
		ifFalse: [ super message ]