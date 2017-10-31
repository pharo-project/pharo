tests - lines
testLinesDo
	| sampleCRString sampleLFString sampleCRLFString lines |
	sampleCRString := 'Fred', String cr , 'the' , String cr , 'Bear'.
	sampleLFString := 'Fred', String lf , 'the' , String lf , 'Bear'.
	sampleCRLFString := 'Fred', String crlf , 'the' , String crlf , 'Bear'.
	
	lines := OrderedCollection new.
	sampleCRString linesDo: [:aLine |
		self deny: (aLine includes: Character cr).
		self deny: (aLine includes: Character lf).
		lines add: aLine].
	self assert: lines asArray = #('Fred' 'the' 'Bear').
	
	lines := OrderedCollection new.
	sampleLFString linesDo: [:aLine |
		self deny: (aLine includes: Character cr).
		self deny: (aLine includes: Character lf).
		lines add: aLine].
	self assert: lines asArray = #('Fred' 'the' 'Bear').

	lines := OrderedCollection new.
	sampleCRLFString linesDo: [:aLine |
		self deny: (aLine includes: Character cr).
		self deny: (aLine includes: Character lf).
		lines add: aLine].
	self assert: lines asArray = #('Fred' 'the' 'Bear').

