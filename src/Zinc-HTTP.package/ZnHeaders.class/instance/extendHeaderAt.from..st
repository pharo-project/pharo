private
extendHeaderAt: key from: line
	"The value of a continuation header line is concatenated, 
	keeping the whitespace, but without the CRLF"

	| existingValue newValue |
	existingValue := self at: key.
	newValue := existingValue isArray
		ifTrue: [ | last |
			last := existingValue size.
			existingValue at: last put: ((existingValue at: last) , line).
			existingValue ]
		ifFalse: [ existingValue , line ].
	self at: key put: newValue