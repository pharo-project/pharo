private
indent: delta fromStream: inStream toStream: outStream
	"Append the contents of inStream to outStream, adding or deleting delta or -delta
	 tabs at the beginning, and after every CR except a final CR.  Do not add tabs
	 to totally empty lines, and be sure nothing but tabs are removed from lines."

	| ch skip cr tab prev atEnd |
	cr := Character cr.
	tab := Character tab.
	delta > 0
		ifTrue: "shift right"
			[prev := cr.
			 [ch := (atEnd := inStream atEnd) ifTrue: [cr] ifFalse: [inStream next].
			  (prev == cr and: [ch ~~ cr]) ifTrue:
				[delta timesRepeat: [outStream nextPut: tab]].
			  atEnd]
				whileFalse:
					[outStream nextPut: ch.
					prev := ch]]
		ifFalse: "shift left"
			[skip := delta. "a negative number"
			 [inStream atEnd] whileFalse:
				[((ch := inStream next) == tab and: [skip < 0]) ifFalse:
					[outStream nextPut: ch].
				skip := ch == cr ifTrue: [delta] ifFalse: [skip + 1]]]