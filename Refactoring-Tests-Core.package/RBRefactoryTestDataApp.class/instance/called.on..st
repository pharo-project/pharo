inline
called: anObject on: aBlock 
	Transcript
		show: anObject printString;
		cr.
	aBlock value