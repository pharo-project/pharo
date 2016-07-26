comparing
= aMatrixTransform2x3 
	| length |
	<primitive: 'primitiveEqual' module: 'FloatArrayPlugin'>
	self class = aMatrixTransform2x3 class ifFalse: [ ^ false ].
	length := self size.
	length = aMatrixTransform2x3 size ifFalse: [ ^ false ].
	1 
		to: self size
		do: [ :i | (self at: i) = (aMatrixTransform2x3 at: i) ifFalse: [ ^ false ] ].
	^ true