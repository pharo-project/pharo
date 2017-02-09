tests
testStrings
	| strings |
	strings := Collection allSubclasses 
		collect: [ :each | each name asString ].
	self serializeAndMaterialize: strings.
	strings := { 
		'foo'. 'Foo BAR'. ''. '	\\'''. 
		'élève en Français'. 
		String with: (Character codePoint: 12354) "HIRAGANA LETTER A" }.
	self serializeAndMaterialize: strings.