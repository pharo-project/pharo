private
processByteOrderMark: codePoint
	^ (codePoint = 16rFEFF or: [ codePoint = 16rFFFE0000 ])
		ifTrue: [ 
			codePoint = 16rFFFE0000
				ifTrue: [ self swapEndianness ].
			true ]
		ifFalse: [ false ]