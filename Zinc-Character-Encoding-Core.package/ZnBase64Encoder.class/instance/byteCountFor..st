private
byteCountFor: string	
	| n byteCount |
	"This assumes there are no line breaks in string"
	n := string size.
	byteCount := n // 4 * 3.
	(n > 1 and: [ (string at: n) = $= ])
		ifTrue: [ 
			(n > 2 and: [ (string at: n - 1) = $= ])
				ifTrue: [ byteCount := byteCount - 2 ]
				ifFalse: [ byteCount := byteCount - 1 ] ].
	^ byteCount