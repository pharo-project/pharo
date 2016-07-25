enumeration
keysDo: aBlock
	^ self keysAndValuesDo: [ :key :value |
		aBlock value: key ]