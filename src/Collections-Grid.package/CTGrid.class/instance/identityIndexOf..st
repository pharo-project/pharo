access
identityIndexOf: anElement
	
	^ self identityIndexOf: anElement ifAbsent: [ 0@0 ]
