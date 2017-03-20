private
generateArguments: aMetalink
	| arguments |
	
	arguments := aMetalink arguments collect: [: each | self genReification: each for: aMetalink].
	(self hasOption: #argsAsArray for: aMetalink) ifTrue: 
		[ ^RBArrayNode statements: arguments ].
	^arguments