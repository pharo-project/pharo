Extension { #name : #Array2D }

{ #category : #'*Collections-arithmetic' }
Array2D >> +* aCollection [
	"Premultiply aCollection by self.  aCollection should be an Array or Matrix.
	 The name of this method is APL's +.x squished into Smalltalk syntax."

	^aCollection preMultiplyByMatrix: self

]

{ #category : #'*Collections-arithmetic' }
Array2D >> preMultiplyByArray: a [
	"Answer a +* self where a is an Array."

	numberOfRows = 1 ifFalse: [self error: 'dimensions do not conform'].
	^Array2D rows: a size columns: numberOfColumns tabulate: [:row :col |
		(a at: row) * (contents at: col)]

]

{ #category : #'*Collections-arithmetic' }
Array2D >> preMultiplyByMatrix: m [
	"Answer m +* self where m is a Matrix."
	|s|
	numberOfRows = m numberOfColumns ifFalse: [self error: 'dimensions do not conform'].
	^ Array2D 
		rows: m numberOfRows 
		columns: numberOfColumns 
		tabulate: [:row :col |
			s := 0.
			1 to: numberOfRows do: [:k | s := (m at: row at: k) * (self at: k at: col) + s].
			s ]
]
