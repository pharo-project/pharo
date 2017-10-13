private
beginTestCase: aTestCase runBlock: aBlock
	| time |
	progressFile 
		nextPutAll: 'starting testcase: ' ; 
		nextPutAll: aTestCase class name; 
		nextPutAll:'>>';
		nextPutAll: aTestCase selector; 
		nextPutAll: ' ... '; 
		flush.
		
	time := aBlock timeToRun.
	stream tab; 
		nextPutAll: '<testcase classname="'; 
		nextPutAll: (self encode: (aTestCase class category copyReplaceAll: '-' with: '.')); 
		nextPut: $.; nextPutAll: (self encode: aTestCase class name); 
		nextPutAll: '" name="'; nextPutAll: (self encode: aTestCase selector); 
		nextPutAll: '" time="'; print: (time asMilliSeconds / 1000.0); 
		nextPutAll: '">'; lf
	
