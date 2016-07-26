private
characterCountFor: bytes
	| n characterCount | 
	n := bytes size.
	characterCount := n // 3 + (n \\ 3) sign * 4.
	^ lineLength isNil
		ifTrue: [ characterCount ]
		ifFalse: [ characterCount + ((characterCount // lineLength ) * lineEnd size) ]