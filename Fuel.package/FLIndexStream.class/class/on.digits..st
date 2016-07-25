instance creation
on: aStream digits: aNumberOfDigits

	^ self basicNew 
		initializeOn: aStream 
		digits: aNumberOfDigits;
		yourself.

	