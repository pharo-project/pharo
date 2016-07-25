results
literals
	literals := literals asArray.	"Put the optimized selectors in literals so as to browse senders more easily"
	additionalLiterals := additionalLiterals asArray reject: [ :e | literals hasLiteral: e ].
	additionalLiterals isEmpty
		ifFalse: [ 
			"Use one entry per literal if enough room, else make anArray"
			literals := literals size + additionalLiterals size + 2 > 255
				ifTrue: [ literals copyWith: additionalLiterals ]
				ifFalse: [ literals , additionalLiterals ] ].
	(literals anySatisfy: [ :each | each isMethodProperties ])
		ifFalse: [ literals := literals copyWith: nil ].
	^ lastLiteral ifNil: [ literals copyWith: nil ] ifNotNil: [ literals copyWith: lastLiteral ]