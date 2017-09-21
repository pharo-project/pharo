private
printToDoNotice
	Transcript
		cr;
		show: 'Level ';
		show: backlinks size printString;
		show: ' with ';
		show: objectsLeft size printString;
		show: ' objects to search'