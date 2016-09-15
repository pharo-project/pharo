strings
pillWith: aString 
	
	^ (self wrapping: aString)
		borderWidth: 1;
		borderColor: (self textColor alpha: 0.5);
		useRoundedCorners;
		yourself