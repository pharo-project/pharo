initialization
vectors: theVectors
	| currentVector |

	self
		assert: [ theVectors size >= 4 ]
		description: [ 'There must be at least four vectors (rectangular area)!' ].

	vector := theVectors first.
	currentVector := vector.

	2 to: theVectors size do: [ :anIndex |
		| aVector |
		aVector := theVectors at: anIndex.
		currentVector next: aVector.
		aVector previous: currentVector.
		currentVector := aVector ].

	currentVector next: vector.
	vector previous: currentVector.

	self computeVertices