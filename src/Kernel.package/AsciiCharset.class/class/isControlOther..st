character classification
isControlOther: char
	char charCode <= 16r1F ifTrue: [ ^ true ].
	^ char = Character delete 