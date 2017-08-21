reading
readInt
	| byteArray n |
	byteArray := stream next: self intSize.
	n := byteArray at: 4.
	n := (n bitShift: 8) + (byteArray at: 3).
	n := (n bitShift: 8) + (byteArray at: 2).
	n := (n bitShift: 8) + (byteArray at: 1).
	(byteArray at: 4) >= 128
		ifTrue: [ n := -16r100000000 + n ].
	^ n