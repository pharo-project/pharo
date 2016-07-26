converting
asUnicode
	| table charset value |
	self leadingChar = 0
		ifTrue: [ ^ self asInteger ].
	charset := self characterSet.
	charset isCharset
		ifFalse: [ ^ self charCode ].
	table := charset ucsTable.
	table ifNil: [ ^ 65533 ].
	value := table at: self charCode + 1.
	value = -1
		ifTrue: [ ^ 65533 ].
	^ value