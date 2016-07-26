accessing
configurationBlock
	^ configurationBlock ifNil: [ configurationBlock := [ :scrolledTextMorph | ] ]