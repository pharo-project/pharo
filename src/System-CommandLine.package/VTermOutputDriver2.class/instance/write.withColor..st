text coloring
write: aString withColor: aColor
	"Print aString with the current style and with aColor font without modifying the current style"

	| newStyle |
	newStyle := self style copy.
	newStyle font color: aColor.
	self
		useStyle: newStyle
		during: [ self nextPutAll: aString ]