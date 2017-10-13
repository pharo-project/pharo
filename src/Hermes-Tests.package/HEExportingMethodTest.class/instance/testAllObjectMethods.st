tests
testAllObjectMethods
	| olds news |
	olds := Object methods collect: [ :e | HEMethod for: e ].
	olds
		do: [ :e | 
			e writeInto: writer.
			writer flush ].

	news := (1 to: olds size) collect: [ :idx | HEMethod readFrom: reader ].
	self assert: olds equals: news