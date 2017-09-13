running
check: aNode forCritiquesDo: aCriticBlock
	| messageReceiver |

	aNode isMessage ifFalse: [ ^ self ].
	
	messageReceiver := aNode receiver.
	messageReceiver isVariable ifFalse: [ ^ self ].
	

	((Smalltalk globals
		at: messageReceiver name asSymbol
		ifAbsent: [ ^ self ])
			respondsTo: aNode selector)
				ifTrue: [ ^ self ].
		
	aCriticBlock cull: (self critiqueFor: aNode)