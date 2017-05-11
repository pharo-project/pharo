private
computeVertices
	vector ifNil: [ ^ self ].
	
	vertices := Array streamContents: [ :aStream |
		| currentVector vertex |
		currentVector := vector.
		vertex := origin.
		aStream nextPut: vertex.
		
		[ currentVector next ~= vector ] whileTrue: [ 
			vertex := currentVector + vertex.
			aStream nextPut: vertex.
			currentVector := currentVector next ] ]