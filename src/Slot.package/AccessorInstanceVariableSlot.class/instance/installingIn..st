class building
installingIn: aClass
	| reader writer | 
	
	reader := String streamContents: [ :str |
		str 
			nextPutAll: self name;
			cr;tab;
			nextPutAll: ' ^';
			nextPutAll: self name.
		 ].
	writer := String streamContents: [ :str |
		str 
			nextPutAll: self name;
			nextPutAll: ': anObject';
			cr;tab;
			nextPutAll: self name;
			nextPutAll: ':= anObject.'.
		].
	aClass compile: reader classified: 'accessing'.
	aClass compile: writer classified: 'accessing'.