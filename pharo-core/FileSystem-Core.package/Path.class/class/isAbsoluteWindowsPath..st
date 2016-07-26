private
isAbsoluteWindowsPath: aString
	aString ifEmpty: [ ^ false ].
	(aString first = $\) ifTrue: [ ^ true ]. "e.g. \file"
	^ ((aString size > 2) and: [ aString first isLetter ])
		ifTrue: [ (aString second = $:) and: [aString third = $\] ] "e.g. C:\..."
		ifFalse: [ false ]
