adding
add: aGTEventCollector
	^ mutex critical: [ 
		(collectors identityIncludes: aGTEventCollector)
			ifFalse: [ collectors add: aGTEventCollector ]
			ifTrue: [ aGTEventCollector ] ]