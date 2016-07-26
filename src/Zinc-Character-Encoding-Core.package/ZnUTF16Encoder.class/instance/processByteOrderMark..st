private
processByteOrderMark: word
	^ (word = 16rFEFF or: [ word = 16rFFFE ])
		ifTrue: [ 
			word = 16rFFFE
				ifTrue: [ self swapEndianness ].
			true ]
		ifFalse: [ false ]