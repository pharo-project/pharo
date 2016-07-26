transformations
scaleBy: aScalarOrPoint
	| double |
	aScalarOrPoint isPoint
		ifTrue:[
			self
				scaleX: aScalarOrPoint x asFloat
				Y: aScalarOrPoint y asFloat ]
		ifFalse:[ 
			double := aScalarOrPoint asFloat.
			self scaleX: double Y: double ].
