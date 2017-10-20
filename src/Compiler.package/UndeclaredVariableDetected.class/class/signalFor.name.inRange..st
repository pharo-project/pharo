instance creation
signalFor: aParser name: aString inRange: anInterval 
	^ (self new setParser: aParser name: aString range: anInterval) signal