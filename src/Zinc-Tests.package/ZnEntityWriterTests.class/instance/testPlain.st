testing
testPlain
	| entity writer output |
	entity := ZnEntity text: 'Foo bar!'.
	output := ByteArray streamContents: [ :out |
		writer := ZnEntityWriter new
			stream: out;
			headers: ZnHeaders new;
			yourself.
		writer writeEntity: entity ].
	self assert: output equals: 'Foo bar!' asByteArray