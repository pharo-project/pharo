submorphs-accessing
wrappingPolicyLabel
	^ self paragraphProvider wrapped
				ifTrue: [ 'W' ]
				ifFalse: [ 'NW' ]