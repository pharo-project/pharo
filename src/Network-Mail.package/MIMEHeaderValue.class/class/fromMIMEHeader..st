instance creation
fromMIMEHeader: aString 
	"This is the value of a MIME header field and so is parsed to extract the various parts"
	| parts newValue parms |
	newValue := self new.
	parts := (aString findTokens: ';') readStream.
	newValue mainValue: parts next.
	parms := Dictionary new.
	parts do: 
		[ :e | | separatorPos parmValue parmName | 
		separatorPos := e 
			findAnySubstring: '='
			startingAt: 1.
		separatorPos <= e size ifTrue: 
			[ parmName := (e 
				copyFrom: 1
				to: separatorPos - 1) trimBoth asLowercase.
			parmValue := (e 
				copyFrom: separatorPos + 1
				to: e size) trimBoth withoutQuoting.
			parms 
				at: parmName
				put: parmValue ] ].
	newValue parameters: parms.
	^ newValue