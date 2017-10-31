tests
testTranscriptStreamingProtocol

	| transcript output |
	
	output := TestableTranscriptOutput new.
	
	transcript := TranscriptStream new.
	
	transcript addOutput: output.
	
	transcript 
	<< 'Hellow';
	clear;
	cr;
	crShow: 'crShow';
	nextPut: $_;
	flush;
	nextPutAll: 'nextPutAll';
	endEntry;
	print: 'print';
	show: '_show';
	space;
	tab;
	close.

	self assert: output data = 'Hellow

crShow_nextPutAllprint_show 	'.
	
	self assert: output apiCount = { 1 . 6 . 1 . 1 . 0 }.
	