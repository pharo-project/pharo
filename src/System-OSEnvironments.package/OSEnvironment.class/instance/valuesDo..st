enumeration
valuesDo: aBlock
	^ self keysAndValuesDo: [ :key :value |
		aBlock value: value ]