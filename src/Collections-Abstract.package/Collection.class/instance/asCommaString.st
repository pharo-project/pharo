printing
asCommaString
	"Return collection printed as 'a, b, c' "

	^String streamContents: [:s | self asStringOn: s delimiter: ', ']
		