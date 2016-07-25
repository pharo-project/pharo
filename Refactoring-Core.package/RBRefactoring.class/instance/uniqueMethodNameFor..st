private
uniqueMethodNameFor: anInteger 
	| before after index name |
	before := 'a'.
	after := ''.
	anInteger timesRepeat: [after := after , 'z:'].
	index := 0.
	
	[name := before , index printString , after.
	(Symbol findInterned: name) notNil] 
			whileTrue: [index := index + 1].
	^name asSymbol