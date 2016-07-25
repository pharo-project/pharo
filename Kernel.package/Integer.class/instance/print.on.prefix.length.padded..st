private
print: positiveNumberString on: aStream prefix: prefix length: minimum padded: zeroFlag
	| padLength |
	padLength := minimum - positiveNumberString size - prefix size.
	padLength > 0
		ifTrue: [zeroFlag
				ifTrue: [aStream nextPutAll: prefix; nextPutAll: (String new: padLength withAll: $0)]
				ifFalse: [aStream nextPutAll: (String new: padLength withAll: Character space); nextPutAll: prefix]]
		ifFalse: [aStream nextPutAll: prefix].
	aStream nextPutAll: positiveNumberString
	