type of methods
asHistorical

	"Sets the receiver as historical object, which will allow itself to retrieve its data using the sourcePointer"
	self annotationNamed: self class statusKey put: #historical.
	self sourcePointer ifNil:[ | pointer compiledMethod |
		pointer := 0.
		compiledMethod := self compiledMethod.
		compiledMethod notNil ifTrue: [
			pointer := compiledMethod sourcePointer ].
		pointer isZero
			ifFalse:[ self sourcePointer: pointer ] ]