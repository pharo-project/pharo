instance creation
replaceFrom: startInteger to: stopInteger with: aString 
	^(self new)
		startPosition: startInteger;
		stopPosition: stopInteger;
		string: aString;
		yourself