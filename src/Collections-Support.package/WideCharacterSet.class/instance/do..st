enumerating
do: aBlock 
	byteArrayMap withIndexDo: [ :each :index | (each = 1) 
			ifTrue: [ aBlock value: (Character value: index - 1) ]].
	map
		keysAndValuesDo: [:index :lowmap |
			| high16Bits |
			high16Bits := index bitShift: 16.
			self
				bitmap: lowmap
				do: [:low16Bits | aBlock value: (Character value: high16Bits + low16Bits)]]