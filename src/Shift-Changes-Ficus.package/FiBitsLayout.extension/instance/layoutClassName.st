*Shift-Changes-Ficus
layoutClassName
	^ self isWordsLayout
		ifTrue: [ #WordLayout ]
		ifFalse: [ #ByteLayout ]