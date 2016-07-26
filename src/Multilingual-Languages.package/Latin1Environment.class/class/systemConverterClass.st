subclass responsibilities
systemConverterClass
	Smalltalk os isWin32
		ifTrue: [^ UTF8TextConverter].
 	Smalltalk os isMacOSX
		ifTrue: [ ^ UTF8TextConverter ].
	Smalltalk os isMacOS
		ifTrue: [^MacRomanTextConverter].
       Smalltalk os isUnix
              ifTrue: [^ UTF8TextConverter].
       ^ Latin1TextConverter 