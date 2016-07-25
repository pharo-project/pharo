initialization
alphabet: string
	"Set the alphabet to use to string, containing 64 characters to represent 64 byte values.
	I automatically compute the inverse used for fast decoding."
	
	self assert: string size = 64 description: '64 characters are needed for a Base64 alphabet'.
	alphabet := string.
	inverse := Array new: 128.
	0 to: 127 do: [ :each | 
		| offset |
		offset := alphabet indexOf: each asCharacter ifAbsent: [ nil ].
		inverse at: each + 1 put: (offset ifNotNil: [ offset - 1 ]) ]