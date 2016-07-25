*System-Changes
nextPreamble
	"Assuming that preamble part does not contain ]lang[ tag"
	| terminator out ch |
	terminator := $!.
	out := (String new: 1000) writeStream.
	self skipSeparators.
	[ (ch := self next) isNil ]
		whileFalse: [ 
			ch == terminator
				ifTrue: [ 
					self peek == terminator
						ifTrue: [ self next	"skip doubled terminator" ]
						ifFalse: [ ^out contents	"terminator is not doubled; we're done!" ] ].
			out nextPut: ch ].
	^ out contents