conversion
nextValue
	"The next six bits of data char from the mimeStream, or nil.  Skip all other chars"
	| raw num |
	[raw := mimeStream next.
	raw ifNil: [^ nil].	"end of stream"
	raw == $= ifTrue: [^ nil].
	num := FromCharTable at: raw asciiValue + 1.
	num ifNotNil: [^ num].
	"else ignore space, return, tab, ..."
	true] whileTrue.