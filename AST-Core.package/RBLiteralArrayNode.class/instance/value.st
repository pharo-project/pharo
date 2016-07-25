accessing
value
	| array |
	array := (isByteArray ifTrue: [ByteArray] ifFalse: [Array]) 
				new: contents size.
	1 to: contents size
		do: [:each | array at: each put: (contents at: each) value].
	^array