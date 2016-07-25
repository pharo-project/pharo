transforming
transform
	class removeClassVariable: variableName.
	destinationClass isNil ifTrue: [^self].
	destinationClass addClassVariable: variableName