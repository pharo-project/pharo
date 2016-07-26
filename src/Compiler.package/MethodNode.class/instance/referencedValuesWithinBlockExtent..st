code generation (closures)
referencedValuesWithinBlockExtent: anInterval 
	^(localsPool select:
		[:temp|
		 temp isReferencedWithinBlockExtent: anInterval]) collect:
			[:temp|
			temp isRemote ifTrue: [temp remoteNode] ifFalse: [temp]]