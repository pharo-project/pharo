instance creation
defaultOriginOf: aPaneName
	^ self new 
		paneName: aPaneName;
		portName: self defaultOriginPortName