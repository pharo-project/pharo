examples
exampleHugeText

	| text |
	text := TxModel new.
	1 to: 100000 do: [ :i | 
		text endPosition
			insert: i asString , ' The quick brown fox jumps over the lazy dog 1234567890';
			insert: String cr.
	].
	^ self openInWindowWithText: text