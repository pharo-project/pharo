private-decoding
skipBitData
	| misc blocksize |
	self readWord.	"skip Image Left"
	self readWord.	"skip Image Top"
	self readWord.	"width"
	self readWord.	"height"
	misc := self next.
	(misc bitAnd: 128) = 0 ifFalse: 
		[ "skip colormap"
		1 
			to: (1 bitShift: (misc bitAnd: 7) + 1)
			do: 
				[ :i | 
				self
					next;
					next;
					next ] ].
	self next.	"minimum code size"
	[ (blocksize := self next) > 0 ] whileTrue: [ self next: blocksize ]