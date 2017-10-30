tests - lines
testLines
	| sampleCRString sampleLFString sampleCRLFString sampleMixedString sampleMixedString2 |
	sampleCRString := 'Fred', String cr , 'the' , String cr , 'Bear'.
	sampleLFString := 'Fred', String lf , 'the' , String lf , 'Bear'.
	sampleCRLFString := 'Fred', String crlf , 'the' , String crlf , 'Bear'.
	sampleMixedString := 'Fred', String lf , 'the' , String cr , 'Bear'.
	sampleMixedString2 := 'Fred', String crlf , 'the' , String cr , 'Bear'.
	
	self assert: sampleCRString lines = #('Fred' 'the'  'Bear').
	self assert: sampleLFString lines = #('Fred' 'the'  'Bear').
	self assert: sampleCRLFString lines = #('Fred' 'the'  'Bear').
	self assert: sampleMixedString lines = #('Fred' 'the'  'Bear').
	self assert: sampleMixedString2 lines = #('Fred' 'the'  'Bear').