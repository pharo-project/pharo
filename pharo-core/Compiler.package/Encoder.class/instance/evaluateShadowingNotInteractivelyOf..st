temps
evaluateShadowingNotInteractivelyOf: aNode 
	self isAnalyzedMethodDefinedByATrait ifTrue: [^false].
	aNode isTemp
				ifTrue: [(aNode scope >= 0) ifTrue: [ ^self notify:'Name is already defined'] ]
				ifFalse: [ self warnAboutShadowed: aNode name ]
