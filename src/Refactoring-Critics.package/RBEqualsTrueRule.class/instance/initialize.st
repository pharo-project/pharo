initialization
initialize
	super initialize.
	self matcher 
		matchesAnyOf: #('true' 'false')
		do: [ :node :answer | 
			answer isNil 
				ifTrue: [
					(node parent isMessage and: [ #(#= #== #~= #~~) includes: node parent selector ]) 
						ifTrue: [ node ]
						ifFalse: [ nil ] ]
				ifFalse: [ answer ] ]