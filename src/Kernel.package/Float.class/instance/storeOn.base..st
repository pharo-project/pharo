printing
storeOn: aStream base: base 
	"Print the Number exactly so it can be interpreted back unchanged"
	self isFinite
		ifTrue: [self sign = -1 ifTrue: [aStream nextPutAll: '-'].
			base = 10 ifFalse: [aStream print: base; nextPut: $r].
			self = 0.0
				ifTrue: [aStream nextPutAll: '0.0']
				ifFalse: [self abs absPrintExactlyOn: aStream base: base]]
		ifFalse: [self isNaN
				ifTrue: [aStream nextPutAll: 'Float nan']
				ifFalse: [self > 0.0
						ifTrue: [aStream nextPutAll: 'Float infinity']
						ifFalse: [aStream nextPutAll: 'Float infinity negated']]]