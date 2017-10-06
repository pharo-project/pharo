instance creation
scanFrom: strm
	"read a doit in the funny format used by Text styles on files. d10 factorial;;  end with two semicolons"

	| pos end doit |
	pos := strm position.
	[strm skipTo: $;. strm peek == $;] whileFalse.
	end := strm position - 1.
	strm position: pos.
	doit := strm next: end-pos.
	strm skip: 2.  ";;"
	^ self evalString: doit