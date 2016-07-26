instance creation
fromBytes: aByteArray pointSize: anInteger  index: i
	^self new
		setFace: (FreeTypeFace fromBytes: aByteArray index: i) pointSize: anInteger; 
		yourself