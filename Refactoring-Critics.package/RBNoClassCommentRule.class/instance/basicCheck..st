running
basicCheck: aClass
	(aClass isMeta or: [ aClass isTestCase ])
		ifTrue: [ ^ false ].
	^ aClass hasComment not