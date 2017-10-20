tests - lines
testLineCorrespondingToIndex
	| sampleCRString sampleLFString sampleCRLFString anIndex |
	sampleCRString := 'Fred', String cr , 'the' , String cr , 'Bear'.
	sampleLFString := 'Fred', String lf , 'the' , String lf , 'Bear'.
	sampleCRLFString := 'Fred', String crlf , 'the' , String crlf , 'Bear'.
	
	anIndex := sampleCRString indexOf: $h.
	self assert: (sampleCRString lineCorrespondingToIndex: anIndex) = 'the'.
	anIndex := sampleLFString indexOf: $h.
	self assert: (sampleLFString lineCorrespondingToIndex: anIndex) = 'the'.
	anIndex := sampleCRLFString indexOf: $h.
	self assert: (sampleCRLFString lineCorrespondingToIndex: anIndex) = 'the'.
	
	anIndex := sampleCRString indexOf: $B.
	self assert: (sampleCRString lineCorrespondingToIndex: anIndex) = 'Bear'.
	anIndex := sampleLFString indexOf: $B.
	self assert: (sampleLFString lineCorrespondingToIndex: anIndex) = 'Bear'.
	anIndex := sampleCRLFString indexOf: $B.
	self assert: (sampleCRLFString lineCorrespondingToIndex: anIndex) = 'Bear'.
	
	anIndex := sampleCRString indexOf: $d.
	self assert: (sampleCRString lineCorrespondingToIndex: anIndex) = 'Fred'.
	anIndex := sampleLFString indexOf: $d.
	self assert: (sampleLFString lineCorrespondingToIndex: anIndex) = 'Fred'.
	anIndex := sampleCRLFString indexOf: $d.
	self assert: (sampleCRLFString lineCorrespondingToIndex: anIndex) = 'Fred'.