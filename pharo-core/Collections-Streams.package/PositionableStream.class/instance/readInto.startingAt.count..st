accessing
readInto: aCollection startingAt: startIndex count: n
	"Read n objects into the given collection. 
	Return number of elements that have been read."
	
	| obj |
	0 to: n - 1 do: [ :i |
		(obj := self next) == nil ifTrue: [ ^ i ].
		aCollection at: startIndex + i put: obj ].
	^ n