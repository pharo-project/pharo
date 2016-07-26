type of methods
isHistorical

	"A ring method can be used to point an old version of the receiver, in this case it will use the sourcePointer to retrieve its information"
	^(self annotationNamed: self class statusKey) 
		ifNil:[ false ] 
		ifNotNil:[ :status| status == #historical ]