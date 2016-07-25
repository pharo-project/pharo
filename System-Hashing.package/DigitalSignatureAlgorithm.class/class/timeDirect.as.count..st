testing
timeDirect: aBlock as: aString count: anInteger

	Transcript show: anInteger asStringWithCommas,'  ',
		aString ,' took ',
		(Time millisecondsToRun: aBlock) asStringWithCommas,' ms'; cr
