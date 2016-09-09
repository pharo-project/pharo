statistics
statisticsReport
	"Workspace openContents: (Smalltalk vm statisticsReport) label: 'VM Statistics'"

	| oldSpaceSize youngSpaceSize memorySize freeSize fullGCs fullGCTime incrGCs incrGCTime tenureCount upTime upTime2 fullGCs2 fullGCTime2 incrGCs2 incrGCTime2 tenureCount2 str |
	oldSpaceSize		:= self oldSpace.
	youngSpaceSize		:= self youngSpaceSize.
	tenureCount			:= self tenureCount.
	memorySize			:= self memorySize.
	freeSize				:= self freeOldSpaceSize + (self edenSpaceSize) - youngSpaceSize.
	fullGCs				:= self fullGCCount.
	fullGCTime			:= self totalFullGCTime.
	incrGCs				:= self incrementalGCCount.
	incrGCTime			:= self totalIncrementalGCTime.
 
	upTime := Time millisecondClockValue.
	str := (String new: 1000) writeStream.
	str	<< 'uptime			';
		print: (upTime / 1000 / 60 // 60); nextPut: $h;
		print: (upTime / 1000 / 60 \\ 60) asInteger; nextPut: $m;
		print: (upTime / 1000 \\ 60) asInteger; nextPut: $s; cr.

	str	<< 'memory			'.
	memorySize printWithCommasOn: str.
	str << ' bytes'; cr.
	str	<<	'	old			'.
	oldSpaceSize printWithCommasOn: str.
	str << ' bytes (';
		print: ((oldSpaceSize / memorySize * 100) roundTo: 0.1); << '%)'; cr.
	str	<< '	young		'.
	(youngSpaceSize) printWithCommasOn: str.
	str << ' bytes (';
		print: ((youngSpaceSize / memorySize * 100) roundTo: 0.1); << '%)'; cr.
	str	<< '	used		'.
	youngSpaceSize + oldSpaceSize - freeSize printWithCommasOn: str.
	str << ' bytes (';
		print: ((youngSpaceSize + oldSpaceSize - freeSize / memorySize * 100) roundTo: 0.1); << '%)'; cr.
	str	<< '	free		'.
	freeSize printWithCommasOn: str.
	str << ' bytes (';
		print: ((freeSize / memorySize * 100) roundTo: 0.1); << '%)'; cr.

	str	<< 'GCs				'.
	(fullGCs + incrGCs) printWithCommasOn: str.
	fullGCs + incrGCs > 0 ifTrue: [
		str
			<< ' ('; 
			print: ((upTime / (fullGCs + incrGCs)) roundTo: 1); 
			<< 'ms between GCs)'
	].
	str cr.
	str	<< '	full			';
		print: fullGCs; << ' totalling '.
	fullGCTime printWithCommasOn: str.
	str << 'ms (';
		print: ((fullGCTime / upTime * 100) roundTo: 0.1);
		<< '% uptime)'.
	fullGCs = 0 ifFalse:
		[str	<< ', avg '; print: ((fullGCTime / fullGCs) roundTo: 0.1); << 'ms'].
	str	cr.
	str	<< '	incr		';
		print: incrGCs; << ' totalling '.
	incrGCTime printWithCommasOn: str.
	str << 'ms (';
		print: ((incrGCTime / upTime * 100) roundTo: 0.1);
		<< '% uptime), avg '; print: ((incrGCTime / incrGCs) roundTo: 0.1); << 'ms'; cr.
	str	<< '	tenures		'.
	tenureCount printWithCommasOn: str.
	tenureCount = 0 ifFalse:
		[str << ' (avg '; print: (incrGCs / tenureCount) asInteger; << ' GCs/tenure)'].
	str	cr.

LastStats ifNil: [LastStats := Array new: 6]
ifNotNil: [
	upTime2 := upTime - (LastStats at: 1).
	fullGCs2 := fullGCs - (LastStats at: 2).
	fullGCTime2 := fullGCTime - (LastStats at: 3).
	incrGCs2 := incrGCs - (LastStats at: 4).
	incrGCTime2 := incrGCTime - (LastStats at: 5).
	tenureCount2 := tenureCount - (LastStats at: 6).

	str	<<  'Since last view	'.
	(fullGCs2 + incrGCs2) printWithCommasOn: str.
	fullGCs2 + incrGCs2 > 0 ifTrue: [
		str
			<< ' ('; 
			print: ((upTime2 / (fullGCs2 + incrGCs2)) roundTo: 1); 
			<< 'ms between GCs)'.
	].
	str cr.
	str	<< '	uptime		'; print: ((upTime2 / 1000.0) roundTo: 0.1); << 's'; cr.
	str	<< '	full			';
		print: fullGCs2; << ' totalling '.
	fullGCTime2 printWithCommasOn: str.
	str << 'ms (';
		print: ((fullGCTime2 / upTime2 * 100) roundTo: 0.1);
		<< '% uptime)'.
	fullGCs2 = 0 ifFalse:
		[str	<< ', avg '; print: ((fullGCTime2 / fullGCs2) roundTo: 0.1); << 'ms'].
	str	cr.
	str	<< '	incr		';
		print: incrGCs2; << ' totalling '.
	incrGCTime2 printWithCommasOn: str.
	str << 'ms (';
		print: ((incrGCTime2 / upTime2 * 100) roundTo: 0.1);
		<< '% uptime), avg '.
	incrGCs2 > 0 ifTrue: [
		 str print: ((incrGCTime2 / incrGCs2) roundTo: 0.1); << 'ms'
	].
	str cr.
	str	<< '	tenures		'.
	tenureCount2 printWithCommasOn: str.
	tenureCount2 = 0 ifFalse:
		[str << ' (avg '; print: (incrGCs2 / tenureCount2) asInteger; << ' GCs/tenure)'].
	str	cr.
].
	LastStats at: 1 put: upTime.
	LastStats at: 2 put: fullGCs.
	LastStats at: 3 put: fullGCTime.
	LastStats at: 4 put: incrGCs.
	LastStats at: 5 put: incrGCTime.
	LastStats at: 6 put: tenureCount.

	^ str contents
