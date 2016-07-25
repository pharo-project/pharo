accessing
registeredNameOf: aColor 
	| colorName |
	colorName := #unnamed.
	ColorRegistry keysAndValuesDo: [:key :value|  value = aColor ifTrue: [ colorName := key ] ].
	^ colorName 