printing
printOn: aStream
	"super printOn: aStream."
	aStream 
		nextPutAll:  '{', self locationType asString,'}';
		nextPutAll: '(' , fileContents size asString, ' bytes )';
		nextPutAll: '[',index asString,'] ';
		nextPutAll: familyName asString;
		nextPutAll: ' - ', styleName asString;
		nextPutAll: ' - ', postscriptName asString;		
		nextPutAll: ' ',(bold ifTrue:['B'] ifFalse:['']);
		nextPutAll: ' ',(italic ifTrue:['I'] ifFalse:['']);
		nextPutAll: ' ',(fixedWidth ifTrue:['Monospaced'] ifFalse:['']);
		nextPutAll: ' ',(stretchValue asString);
		nextPutAll: ' ',(weightValue asString);	
		cr