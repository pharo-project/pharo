accessing
parseAuthRequest: headerValue
	| data dict |
	dict := Dictionary new.
	"Chops off the ' Digest ' scheme name, a bit too brittle."
	data := headerValue copyFrom: 8 to: headerValue size.
	(data substrings: ',') do: [ :fragment | | tokens i key value |
		tokens := fragment trimBoth.
		i := tokens indexOf: $=.
		(i > 0) ifTrue: [
			key := tokens copyFrom: 1 to: i - 1.
			value := tokens copyFrom: i + 1 to: tokens size.  
			dict at: key put: (value trimBoth: [ :char | char = $" ]) ] ].
	^ dict