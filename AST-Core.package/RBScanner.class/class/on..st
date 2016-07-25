instance creation
on: aStream 
	| str |
	str := self new on: aStream.
	str
		step;
		stripSeparators.
	^str