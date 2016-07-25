accessing
name: aName
	aName ifNil: [ ^self "for anonymous classes" ].
	[ name := aName asSymbol ]
		on: Error 
		do: [ InvalidGlobalName 
				signal: 'Global names should be symbols' 
				for: aName ].
	self validateClassName.
	self warnOfDangerousClasses.