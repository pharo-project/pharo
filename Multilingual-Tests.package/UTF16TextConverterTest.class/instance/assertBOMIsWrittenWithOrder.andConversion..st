helpers
assertBOMIsWrittenWithOrder: anOrder andConversion: aBlock

	|converter stream  BOM  |
	
	converter := UTF16TextConverter new.
	converter useByteOrderMark: true.
	
	anOrder = #bigEndian ifTrue: [converter useLittleEndian: false].
	anOrder = #littleEndian ifTrue: [converter useLittleEndian: true].
	
	BOM := (converter useLittleEndian 
				ifTrue: [16rFFFE]
				ifFalse: [16rFEFF]) asByteArray asString.
	
	stream := String new writeStream. 
	aBlock value: converter value: stream.
	
	self assert: BOM equals: (stream contents  first: 2).
	