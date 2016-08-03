*Reflectivity
asMethodConst
	| const constNode link |
	"this method will replace full sender message send of sender with me (or my value which usefull for block). So following sender method execution will not evaluate my receiver expresion but instead push precomputed literal with me"
	const := self value.
	constNode := thisContext sender sourceNodeExecuted.
	link := MetaLink new
		metaObject: const;
		control: #instead.
	constNode link: link.
	^const