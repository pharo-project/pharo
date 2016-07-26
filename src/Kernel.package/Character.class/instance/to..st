converting
to: other
	"Answer with a collection in ascii order -- $a to: $z"
	^ (self asciiValue to: other asciiValue) collect:
				[:ascii | Character value: ascii]