collection ops
do: aBlock 
	map
		keysAndValuesDo: [:index :lowmap |
			| high16Bits |
			high16Bits := index bitShift: 16.
			self
				bitmap: lowmap
				do: [:low16Bits | aBlock value: (Character value: high16Bits + low16Bits)]]