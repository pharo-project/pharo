instance creation
on: aStream errorBlock: aBlock 
	| str |
	str := self new on: aStream.
	str
		errorBlock: aBlock;
		step;
		stripSeparators.
	^str