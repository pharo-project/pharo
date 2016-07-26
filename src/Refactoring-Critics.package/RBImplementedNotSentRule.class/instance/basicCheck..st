running
basicCheck: aMethod
	"Check if there are any senders. Furthermore methods with pragmas are likely to be sent through reflection, thus do not report those. Also test methods are sent through reflection, so ignore those as well."
	(self literals includes: aMethod selector)
		ifTrue: [ ^ false ].
	aMethod pragmas isEmpty
		ifFalse: [ ^ false ].
	^ (aMethod methodClass isMeta not
		and: [ aMethod methodClass isTestCase ]) not
			