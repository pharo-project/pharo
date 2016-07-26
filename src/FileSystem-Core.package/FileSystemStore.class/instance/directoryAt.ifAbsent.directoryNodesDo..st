public
directoryAt: aPath ifAbsent: absentBlock directoryNodesDo: aBlock
	^ self 
		directoryAt: aPath 
		ifAbsent: absentBlock 
		nodesDo: [ :entry|
			(self basicIsDirectory: entry) 
				ifTrue: [ aBlock value: entry ]].