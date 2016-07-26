managing wrapping policy
wrappingPolicyLabel
	^ self paragraphProvider wrapped
				ifTrue: [ 'W' ]
				ifFalse: [ 'NW' ]