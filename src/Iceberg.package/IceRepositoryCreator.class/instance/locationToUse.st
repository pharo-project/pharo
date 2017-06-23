private accessing
locationToUse
	^ self location ifNil: [ self defaultLocation ]