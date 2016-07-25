transforming
checkNodes: sequenceNodes 
	(sequenceNodes 
		detect: [:each | RBReadBeforeWrittenTester isVariable: name readBeforeWrittenIn: each]
		ifNone: [nil]) notNil 
		ifTrue: [^false].
	sequenceNodes do: 
			[:each | 
			(self usesDirectly: each body) 
				ifTrue: [blockNodes add: each]
				ifFalse: 
					[(self checkNodes: (self subblocksIn: each body)) 
						ifFalse: [blockNodes add: each]]].
	^true