running
testMorphInline

	| text morphTextAttribute |
	text := 'Hello ' asText, Morph new asText, ' world' asText.

	"Stupid assertion, but this test is mostly a usage example"
	morphTextAttribute := text attributesAt: 7.
	self assert: (morphTextAttribute first isKindOf: TextAnchor)