comparing
hash
	"Hash is reimplemented because = is implemented."

	^(((start hash bitShift: 2)
		bitOr: stop hash)
		bitShift: 1)
		bitOr: self size