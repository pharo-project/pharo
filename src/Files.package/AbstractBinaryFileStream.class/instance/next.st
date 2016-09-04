accessing
next
	"Answer the next byte from this file, or nil if at the end of the file."

	^ (self next: 1) ifEmpty: [ nil ] ifNotEmpty: [ :col | col first ]