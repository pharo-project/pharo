inline
called: anObject on1: aBlock 
	| each |
	each := anObject printString.
	Transcript
		show: each;
		cr.
	aBlock value: each