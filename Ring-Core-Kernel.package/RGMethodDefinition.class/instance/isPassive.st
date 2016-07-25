type of methods
isPassive

	"A ring method isPassive when it retrieves the data that was assigned in its creation.
	By default is passive"
	^(self annotationNamed: self class statusKey) 
		ifNil:[ true ] 
		ifNotNil:[ :status| status == #passive ]