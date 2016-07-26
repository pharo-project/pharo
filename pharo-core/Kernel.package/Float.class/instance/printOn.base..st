printing
printOn: stream base: base
	"Handle sign, zero, and NaNs; all other values passed to FloatPrintPolicy" 

	self isNaN ifTrue: [ ^ stream nextPutAll: 'Float nan' ]. "check for NaN before sign"
	self isInfinite 
		ifTrue: [ 
			stream nextPutAll: 'Float infinity'.
			^ self sign = -1 ifTrue: [ stream nextPutAll: ' negated' ] ].
	self > 0.0
		ifTrue: [ FloatPrintPolicy absPrint: self on: stream base: base ]
		ifFalse: [
			self sign = -1 ifTrue: [ stream nextPut: $- ].
			self = 0.0
				ifTrue: [ stream nextPutAll: '0.0' ]
				ifFalse: [ FloatPrintPolicy absPrint: self negated on: stream base: base ] ]