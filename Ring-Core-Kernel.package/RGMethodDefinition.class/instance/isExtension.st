accessing
isExtension
	"The receiver is an extension when is defined in a different package to the one of its parent.
	Ring allows to set this property. If not assigned tries to find its value "
	
	^self 
		annotationNamed: self class isExtensionKey
		ifAbsent: [  
			(self parent notNil and:[ self package notNil ])
				ifTrue: [ | value |
					       value :=  self parent package ~= self package.
						 self annotationNamed: self class isExtensionKey put: value.
						 value ]
				ifFalse: [ self protocol ifNil:[ false ] ifNotNil:[ :prot|  prot beginsWith: '*' ] ] ]