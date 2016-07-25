reporting
reportGCStatsOn: str
	| oldSpaceEnd youngSpaceEnd memoryEnd fullGCs fullGCTime incrGCs incrGCTime tenureCount upTime rootOverflows |
	upTime := time.
	oldSpaceEnd			:= gcStats at: 1.
	youngSpaceEnd		:= gcStats at: 2.
	memoryEnd			:= gcStats at: 3.
	fullGCs				:= gcStats at: 7.
	fullGCTime			:= gcStats at: 8.
	incrGCs				:= gcStats at: 9.
	incrGCTime			:= gcStats at: 10.
	tenureCount			:= gcStats at: 11.
	rootOverflows		:= gcStats at: 22.

	str cr.
	str	nextPutAll: '**Memory**'; cr.
	str	nextPutAll:	'	old			'.
	oldSpaceEnd printWithCommasSignedOn: str. 
	str nextPutAll: ' bytes'; cr.
	str	nextPutAll: '	young		'.
	(youngSpaceEnd - oldSpaceEnd) printWithCommasSignedOn: str. 
	str  nextPutAll: ' bytes'; cr.
	str	nextPutAll: '	used		'.
	youngSpaceEnd printWithCommasSignedOn: str. 
	str  nextPutAll: ' bytes'; cr.
	str	nextPutAll: '	free		'.
	(memoryEnd - youngSpaceEnd) printWithCommasSignedOn: str. 
	str  nextPutAll: ' bytes'; cr.

	str cr.
	str	nextPutAll: '**GCs**'; cr.
	str	nextPutAll: '	full			';
		print: fullGCs; nextPutAll: ' totalling '.
	fullGCTime printWithCommasOn: str.
	str nextPutAll: 'ms (';
		print: ((fullGCTime / upTime * 100) roundTo: 1.0);
		nextPutAll: '% uptime)'.
	fullGCs = 0 ifFalse:
		[str	nextPutAll: ', avg '; print: ((fullGCTime / fullGCs) roundTo: 1.0); nextPutAll: 'ms'].
	str	cr.
	str	nextPutAll: '	incr		';
		print: incrGCs; nextPutAll: ' totalling '.
	incrGCTime printWithCommasOn: str.
	str nextPutAll: 'ms (';
		print: ((incrGCTime / upTime * 100) roundTo: 1.0);
		nextPutAll: '% uptime)'.
	incrGCs = 0 ifFalse:
		[str nextPutAll:', avg '; print: ((incrGCTime / incrGCs) roundTo: 1.0); nextPutAll: 'ms'].
	str cr.
	str	nextPutAll: '	tenures		'.
	tenureCount printWithCommasOn: str.
	tenureCount = 0 ifFalse:
		[str nextPutAll: ' (avg '; print: (incrGCs / tenureCount) asInteger; nextPutAll: ' GCs/tenure)'].
	str	cr.
	str	nextPutAll: '	root table	'.
	rootOverflows printWithCommasOn: str.
	str nextPutAll:' overflows'.
	str cr.
