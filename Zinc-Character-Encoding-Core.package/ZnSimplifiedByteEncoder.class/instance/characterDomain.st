accessing
characterDomain
	"Return a set with the characters that are in my domain, that I can encode"

	^ ((0 to: 127) asSet, unicodeToByte keys) collect: [ :each | Character value: each  ]