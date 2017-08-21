background coloring
write: aString withBackground: aColor
	"Print aString with the current style and with aColor background without modifying the current style"

	| tempStyle |
	tempStyle := self style copy.
	tempStyle background color: aColor.
	self
		useStyle: tempStyle
		during: [ self nextPutAll: aString ]