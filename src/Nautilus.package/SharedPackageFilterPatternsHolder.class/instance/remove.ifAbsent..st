removing
remove: aString ifAbsent: aBlockClosure 
	
	^ mutex critical: [ value remove: aString ifAbsent: aBlockClosure ]