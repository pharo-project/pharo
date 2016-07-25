type of methods
isActive

	"A ring method isActive when it needs to access the compiledMethod for retrieving its data"
	^(self annotationNamed: self class statusKey) 
		ifNil:[ false ] 
		ifNotNil:[ :status| status == #active ]