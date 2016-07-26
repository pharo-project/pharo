instance creation
defaultTargetOf: aPaneName
	^ self new 
		paneName: aPaneName;
		portName: self defaultTargetPortName