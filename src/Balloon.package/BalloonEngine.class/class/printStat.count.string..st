accessing
printStat: time count: n string: aString
	Transcript
		cr;
		print: time; tab;
		nextPutAll:' mSecs -- ';
		print: n; tab;
		nextPutAll:' ops -- ';
		print: ((time asFloat / (n max: 1) asFloat) roundTo: 0.01); tab;
		nextPutAll: ' avg. mSecs/op -- ';
		nextPutAll: aString.