accessing-packages
packageAt: aName
	^ self packageAt: aName ifAbsent: [ NotFound signalFor: aName in: self ]