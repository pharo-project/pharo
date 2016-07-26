type of comments
isActive

	"A ring comment isActive when it needs to access the class organization for retrieving its data"
	^(self annotationNamed: self class statusKey) 
		ifNil:[ false ] 
		ifNotNil:[ :status| status == #active ]