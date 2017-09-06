running
check: aMethod forCritiquesDo: aCriticBlock
	"Check if there are any senders. Furthermore methods with pragmas are likely to be sent through reflection, thus do not report those. Also test methods are sent through reflection, so ignore those as well."
	
	aMethod pragmas ifNotEmpty: [ ^ self ].
	(aMethod methodClass isMeta not and: [
	aMethod methodClass isTestCase ]) ifTrue: [ ^ self ].
	
	(self class allMessages includes: aMethod selector)
		ifTrue: [ ^ self ].
	
	aCriticBlock cull: (self critiqueFor: aMethod)