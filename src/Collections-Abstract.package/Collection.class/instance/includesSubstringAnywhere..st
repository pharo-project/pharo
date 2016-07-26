testing
includesSubstringAnywhere: testString
	"Answer whether the receiver includes, anywhere in its nested structure, a string that has testString as a substring"
	self do:
		[:element |
			(element isString)
				ifTrue:
					[(element includesSubstring: testString) ifTrue: [^ true]].
			(element isCollection)
				ifTrue:
					[(element includesSubstringAnywhere: testString) ifTrue: [^ true]]].
	^ false

"#(first (second third) ((allSentMessages ('Elvis' includes:)))) includesSubstringAnywhere:  'lvi'"