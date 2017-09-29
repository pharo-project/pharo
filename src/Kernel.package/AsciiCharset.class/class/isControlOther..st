character classification
isControlOther: char 
	| code |
	code := char charCode.
	code <= 16r1F ifTrue: [ ^ true ].
	^ code = Character delete