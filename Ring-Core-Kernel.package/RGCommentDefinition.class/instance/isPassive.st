type of comments
isPassive

	"A ring comment isPassive by default.  In this case it will retrieve the data that was assigned in its creation"
	^(self annotationNamed: self class statusKey) 
		ifNil:[ true ] 
		ifNotNil:[ :status| status == #passive ]