signalling
signal: aString with: aBlock 
	^ self new
		actionBlock: aBlock;
		signal: aString