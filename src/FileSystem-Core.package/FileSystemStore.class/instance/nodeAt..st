public
nodeAt: aPath
	^ self 
		nodeAt: aPath 
		ifPresent: [ :node| node ]
		ifAbsent: [ NotFound signalFor: aPath in: self ]