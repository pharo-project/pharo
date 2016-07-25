private
charFromStream: aStream withFirst: firstValue

	^(firstValue < 16rD800 or: [firstValue > 16rDBFF])
		ifTrue:
			[Unicode value: firstValue]
		ifFalse:  "We have a surrogate pair"
			[| character1 character2 n secondValue |
			character1 := aStream basicNext ifNil: [^nil].
			character2 := aStream basicNext ifNil: [^nil].
			self useLittleEndian ifTrue: [ |tmp|
				tmp := character1.
				character1 := character2.
				character2 := tmp].
			secondValue := (character1 charCode << 8) + (character2 charCode).
			Unicode value: (firstValue - 16rD800) * 16r400 + (secondValue - 16rDC00) + 16r10000.]
