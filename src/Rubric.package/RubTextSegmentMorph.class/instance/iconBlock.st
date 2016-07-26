accessing
iconBlock
	^ iconBlock ifNil: [ iconBlock := [ :seg :evt | ] ]