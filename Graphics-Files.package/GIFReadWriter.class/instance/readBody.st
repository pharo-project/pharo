private-decoding
readBody
	"Read the GIF blocks. Modified to return a form.  "
	| form extype block blocksize packedFields delay1 |
	form := nil.
	[ stream atEnd ] whileFalse: 
		[ block := self next.
		block = Terminator ifTrue: [ ^ form ].
		block = ImageSeparator 
			ifTrue: 
				[ form
					ifNil: [ form := self readBitData ]
					ifNotNil: [ self skipBitData ] ]
			ifFalse: 
				[ block = Extension ifFalse: [ ^ form	"^ self error: 'Unknown block type'" ].
				"Extension block"
				extype := self next.	"extension type"
				extype = 249 
					ifTrue: 
						[ "graphics control"
						self next = 4 ifFalse: [ ^ form	"^ self error: 'corrupt GIF file'" ].
						"====
				Reserved                      3 Bits
				Disposal Method               3 Bits
				User Input Flag               1 Bit
				Transparent Color Flag        1 Bit
				==="
						packedFields := self next.
						delay1 := self next.	"delay time 1"
						delay := (self next * 256 + delay1) * 10.	"delay time 2"
						transparentIndex := self next.
						(packedFields bitAnd: 1) = 0 ifTrue: [ transparentIndex := nil ].
						self next = 0 ifFalse: [ ^ form	"^ self error: 'corrupt GIF file'" ] ]
					ifFalse: 
						[ "Skip blocks"
						[ (blocksize := self next) > 0 ] whileTrue: 
							[ "Read the block and ignore it and eat the block terminator"
							self next: blocksize ] ] ] ]