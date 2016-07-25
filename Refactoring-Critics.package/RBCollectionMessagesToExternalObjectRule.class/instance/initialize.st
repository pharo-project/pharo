initialization
initialize
	| queries |
	super initialize.
	queries := #( add: remove: addAll: removeAll: )
		collect: [ :each | '(`@Object `@message: `@args) <1s> `@Arg' expandMacrosWith: each ].
	self matcher 
		matchesAnyOf: queries
		do: [ :node :answer |
			answer isNil 
				ifTrue: [
					((node receiver selector copyFrom: 1 to: (node receiver selector size min: 2)) ~= 'as' 
						and: [
							| receiver |
							receiver := node receiver receiver.
							receiver isVariable not 
								or: [ ((#('self' 'super') includes: receiver name) 
								or: [ Smalltalk globals includesKey: receiver name asSymbol ]) not ] ]) 
							ifTrue: [ node ]
							ifFalse: [ nil ] ]
				ifFalse: [ answer ] ]