instance creation
fromFile: aFileName pointSize: anInteger index: i
	^self new
		setFace: (FreeTypeFace fromFile: aFileName index: i) pointSize: anInteger; 
		yourself