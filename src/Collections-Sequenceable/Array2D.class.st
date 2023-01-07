"
I represent a mathematical matrix or a two-dimensional array. I provide methods for creating matrices, operating on them arithmetically and algebraically. 

Structure:
 - numberOfRows : a non-negative integer saying how many rows there are.
 - numberOfColumns : a non-negative integer saying how many columns there are.
 - contents : an Array holding the elements in row-major order.  That is, for a 2x3 array the contents are (11 12 13 21 22 23).  


Element-wise matrix arithmetic works; you can freely mix matrices and numbers but
don't try to mix matrices and arrays (yet).
Matrix multiplication, using the symbol +* (derived from APL's +.x), works between
(Matrix or Array) +* (Matrix or Array).  Don't try to use a number as an argument of +*.
Matrix * Number and Number * Matrix work fine, so you don't need +* with numbers.

"
Class {
	#name : #Array2D,
	#superclass : #Collection,
	#instVars : [
		'contents',
		'numberOfColumns',
		'numberOfRows'
	],
	#category : #'Collections-Sequenceable-Ordered'
}

{ #category : #'special instance creation' }
Array2D class >> columnVector: aCollection [
	"Create a matrix of one column having aCollection as contents"

	^ self rows: aCollection size columns: 1 contents: aCollection asArray shallowCopy
]

{ #category : #'special instance creation' }
Array2D class >> diagonal: aCollection [
	|r i|
	r := self zeros: aCollection size.
	i := 0.
	aCollection do: [:each | i := i+1. r at: i at: i put: each].
	^r
]

{ #category : #'special instance creation' }
Array2D class >> identity: n [
	| r |
	r := self zeros: n.
	1 to: n do: [ :i | r at: i at: i put: 1 ].
	^ r
]

{ #category : #'instance creation' }
Array2D class >> new: dimension [
	"Answer a dimension*dimension matrix."
	^ self rows: dimension columns: dimension
]

{ #category : #'instance creation' }
Array2D class >> new: dimemsion element: element [
	"Answer a dimemsion*dimemsion matrix with all elements set to element."

	^ self rows: dimemsion columns: dimemsion element: element
]

{ #category : #'instance creation' }
Array2D class >> new: dimension tabulate: aTwoArgumentBlock [
	"Answer a dimension*dimension matrix where it at: i at: j is aBlock value: i value: j."
	^ self rows: dimension columns: dimension tabulate: aTwoArgumentBlock
]

{ #category : #'special instance creation' }
Array2D class >> ones: dimension [
	"Create a squared matrix of dimension full of 1"
	^ self new: dimension element: 1
]

{ #category : #'special instance creation' }
Array2D class >> rowVector: aCollection [
	"Create a matrix of one row having aCollection as contents"

	^self rows: 1 columns: aCollection size contents: aCollection asArray shallowCopy
]

{ #category : #'instance creation' }
Array2D class >> rows: rowNumber columns: columnNumber [
	"Create a Matrix of rowNUmber rows and columnNumber columns."
	^ self
		rows: rowNumber
		columns: columnNumber
		contents: (Array new: rowNumber*columnNumber)
]

{ #category : #private }
Array2D class >> rows: rowNumber columns: columnNumber contents: contents [
	"Private! Creates a Matrix of the given size with an adequate contents."
	^ self new rows: rowNumber columns: columnNumber contents: contents
]

{ #category : #'instance creation' }
Array2D class >> rows: rowNumber columns: columnNumber element: element [
	"Create a Matrix of rowNumber rows and columnNumber columns filled with element."
	^ self
		rows: rowNumber
		columns: columnNumber
		contents: ((Array new: rowNumber * columnNumber) atAllPut: element; yourself)
]

{ #category : #'instance creation' }
Array2D class >> rows: rowNumber columns: columnNumber tabulate: aTwoArgumentBlock [
	"Answer a new Matrix of the given dimensions where
	 result at: i at: j is aTwoArgumentBlock value: i value: j"
	|a i|
	a := Array new: rowNumber*columnNumber.
	i := 0.
	1 to: rowNumber do: [:row |
		1 to: columnNumber do: [:column |
			a at: (i := i + 1) put: (aTwoArgumentBlock value: row value: column)]].
	^ self rows: rowNumber columns: columnNumber contents: a
]

{ #category : #'special instance creation' }
Array2D class >> zeros: dimension [
	"Create a Matrix of dimensionxdimemsion"
	^ self new: dimension element: 0
]

{ #category : #arithmetic }
Array2D >> +* aCollection [
	"Premultiply aCollection by self.  aCollection should be an Array or Matrix.
	 The name of this method is APL's +.x squished into Smalltalk syntax."

	^aCollection preMultiplyByMatrix: self
]

{ #category : #copying }
Array2D >> , aMatrix [
	"Answer a new matrix having the same number of rows as the receiver and aMatrix,
	 its columns being the columns of the receiver followed by the columns of aMatrix."
	|newCont newCols anArray oldCols a b c|

	[numberOfRows = aMatrix numberOfRows] assert.
	newCont := Array new: self size + aMatrix size.
	anArray := aMatrix privateContents.
	oldCols := aMatrix numberOfColumns.
	newCols := numberOfColumns + oldCols.
	a := b := c := 1.
	1 to: numberOfRows do: [:r |
		newCont replaceFrom: a to: a + numberOfColumns - 1 with: contents startingAt: b.
		newCont replaceFrom: a+numberOfColumns to: a + newCols - 1 with: anArray startingAt: c.
		a := a + newCols.
		b := b + numberOfColumns.
		c := c + oldCols].
	^self class rows: numberOfRows columns: newCols contents: newCont
]

{ #category : #copying }
Array2D >> ,, aMatrix [
	"Answer a new matrix having the same number of columns as the receiver and aMatrix,
	 its rows being the rows of the receiver followed by the rows of aMatrix."

	[numberOfColumns = aMatrix numberOfColumns] assert.
	^self class
			rows: numberOfRows + aMatrix numberOfRows
			columns: numberOfColumns
			contents: contents , aMatrix privateContents
]

{ #category : #comparing }
Array2D >> = aMatrix [
	^ aMatrix class == self class
		and: [ aMatrix numberOfRows = numberOfRows
				and: [ aMatrix numberOfColumns = numberOfColumns
					and: [ aMatrix privateContents = contents ] ] ]
]

{ #category : #adding }
Array2D >> add: newObject [
	self shouldNotImplement
]

{ #category : #accessing }
Array2D >> anyOne [
	"Return one element from the receiver"
	^ contents anyOne
]

{ #category : #converting }
Array2D >> asArray [
	^ contents shallowCopy
]

{ #category : #converting }
Array2D >> asBag [
	^ contents asBag
]

{ #category : #converting }
Array2D >> asByteArray [
	^ contents asByteArray
]

{ #category : #converting }
Array2D >> asCharacterSet [
	^ contents asCharacterSet
]

{ #category : #converting }
Array2D >> asFloatArray [
	^ contents asFloatArray
]

{ #category : #converting }
Array2D >> asIdentitySet [
	^ contents asIdentitySet
]

{ #category : #converting }
Array2D >> asIntegerArray [
	^ contents asIntegerArray
]

{ #category : #converting }
Array2D >> asOrderedCollection [
	^ contents asOrderedCollection
]

{ #category : #converting }
Array2D >> asSet [
	^ contents asSet
]

{ #category : #converting }
Array2D >> asSortedCollection [
	^ contents asSortedCollection
]

{ #category : #converting }
Array2D >> asSortedCollection: aBlock [
	^ contents asSortedCollection: aBlock
]

{ #category : #converting }
Array2D >> asWordArray [
	^ contents asWordArray
]

{ #category : #accessing }
Array2D >> at: rowNumber at: columnNumber [
	^ contents at: (self indexForRow: rowNumber andColumn: columnNumber)
]

{ #category : #accessing }
Array2D >> at: rowNumber at: columnNumber ifInvalid: value [
	"If rowNumber,columnNumber is a valid index for the receiver, answer the corresponding element. Otherwise, answer value."

	(rowNumber between: 1 and: numberOfRows) ifFalse: [ ^ value ].
	(columnNumber between: 1 and: numberOfColumns) ifFalse: [ ^ value ].
	^contents at: (rowNumber - 1) * numberOfColumns + columnNumber
]

{ #category : #accessing }
Array2D >> at: rowNumber at: columnNumber incrementBy: value [
	"Add a value to the element available at rowNumber,columNumber."

	^ contents at: (self indexForRow: rowNumber andColumn: columnNumber) incrementBy: value
]

{ #category : #accessing }
Array2D >> at: rowNumber at: columnNumber put: value [
	"Put value at rowNumber,columnNumber"
	^ contents at: (self indexForRow: rowNumber andColumn: columnNumber) put: value
]

{ #category : #accessing }
Array2D >> atAllPut: value [
	"Put value as value of all the receiver elements."
	contents atAllPut: value
]

{ #category : #'row/column operations' }
Array2D >> atColumn: column [

	|p|
	p := (self indexForRow: 1 andColumn: column)-numberOfColumns.
	^(1 to: numberOfRows) collect: [:row | contents at: (p := p+numberOfColumns)]
]

{ #category : #'row/column operations' }
Array2D >> atColumn: column put: aCollection [
	| p |
	aCollection size = numberOfRows ifFalse: [ self error: 'wrong column size' ].
	p := (self indexForRow: 1 andColumn: column) - numberOfColumns.
	aCollection do: [ :each | contents at: (p := p + numberOfColumns) put: each ].
	^ aCollection
]

{ #category : #random }
Array2D >> atRandom [
	"Return the contents of the receiver in a random order."
	^ contents atRandom
]

{ #category : #random }
Array2D >> atRandom: aGenerator [
	^ contents atRandom: aGenerator
]

{ #category : #'row/column operations' }
Array2D >> atRow: rowNumber [
	(rowNumber between: 1 and: numberOfRows)
		ifFalse: [self error: '1st subscript out of range'].
	^ contents
			copyFrom: (rowNumber - 1) * numberOfColumns + 1
			to: rowNumber*numberOfColumns
]

{ #category : #'row/column operations' }
Array2D >> atRow: row put: aCollection [
	|p|

	aCollection size = numberOfColumns ifFalse: [self error: 'wrong row size'].
	p := (self indexForRow: row andColumn: 1)-1.
	aCollection do: [:each | contents at: (p := p+1) put: each].
	^aCollection
]

{ #category : #'accessing - submatrices' }
Array2D >> atRows: rs columns: cs [
	"Answer a Matrix obtained by slicing the receiver.
	 rs and cs should be sequenceable collections of positive integers."

	^self class rows: rs size columns: cs size tabulate: [:r :c |
		self at: (rs at: r) at: (cs at: c)]
]

{ #category : #'accessing - submatrices' }
Array2D >> atRows: r1 to: r2 columns: c1 to: c2 [
	"Answer a submatrix [r1..r2][c1..c2] of the receiver."
	|rd cd|

	rd := r1 - 1.
	cd := c1 - 1.
	^self class rows: r2-rd columns: c2-cd tabulate: [:r :c| self at: r+rd at: c+cd]
]

{ #category : #'accessing - submatrices' }
Array2D >> atRows: r1 to: r2 columns: c1 to: c2 ifInvalid: element [
	"Answer a submatrix [r1..r2][c1..c2] of the receiver.
	 Portions of the result outside the bounds of the original matrix are filled in with element."
	|rd cd|
	rd := r1 - 1.
	cd := c1 - 1.
	^self class rows: r2-rd columns: c2-cd tabulate: [ :r :c| self at: r+rd at: c+cd ifInvalid: element]
]

{ #category : #'accessing - submatrices' }
Array2D >> atRows: r1 to: r2 columns: c1 to: c2 put: aMatrix [
	"Set the [r1..r2][c1..c2] submatrix of the receiver
	 from the [1..r2-r1+1][1..c2-c1+1] submatrix of aMatrix.
	 As long as aMatrix responds to at:at: and accepts arguments in the range shown,
	 we don't care if it is bigger or even if it is a Matrix at all."
	|rd cd|

	rd := r1 - 1.
	cd := c1 - 1.
	r1 to: r2 do: [:r |
		c1 to: c2 do: [:c |
			self at: r at: c put: (aMatrix at: r-rd at: c-cd)]].
	^aMatrix
]

{ #category : #enumerating }
Array2D >> collect: aBlock [
	"Answer a new matrix with transformed elements; transformations should be independent."

	^self class
		rows: numberOfRows
		columns: numberOfColumns
		contents: (contents collect: aBlock)
]

{ #category : #accessing }
Array2D >> columnCount [
	^ numberOfColumns
]

{ #category : #'row/column operations' }
Array2D >> diagonal [
	"Answer (1 to: (numberOfRows min: numberOfColumns)) collect: [:i | self at: i at: i]"
	|i|
	i := numberOfColumns negated.
	^ (1 to: (numberOfRows min: numberOfColumns)) collect: [:j | contents at: (i := i + numberOfColumns + 1)]
]

{ #category : #enumerating }
Array2D >> difference: aCollection [
	"Union is in because the result is always a Set.
	 Difference and intersection are out because the result is like the receiver,
	 and with irregular seleection that cannot be."
	self shouldNotImplement
]

{ #category : #enumerating }
Array2D >> do: aBlock [
	"Pass elements to aBlock one at a time in row-major order."
	contents do: aBlock
]

{ #category : #accessing }
Array2D >> extent [
    "Answer the receiver's dimensions as point."

    ^ self numberOfColumns @ self numberOfRows
]

{ #category : #comparing }
Array2D >> hash [
	"I'm really not sure what would be a good hash function here.
	 The essential thing is that it must be compatible with #=, and
	 this satisfies that requirement."

	^contents hash
]

{ #category : #testing }
Array2D >> identityIncludes: anObject [
	^ contents identityIncludes: anObject
]

{ #category : #accessing }
Array2D >> identityIndexOf: anElement [

	^ self identityIndexOf: anElement ifAbsent: [ 0@0 ]
]

{ #category : #accessing }
Array2D >> identityIndexOf: anElement ifAbsent: anExceptionBlock [
	^self rowAndColumnForIndex:
		 (contents identityIndexOf: anElement ifAbsent: [^anExceptionBlock value])
]

{ #category : #testing }
Array2D >> includes: anObject [
	^ contents includes: anObject
]

{ #category : #testing }
Array2D >> includesAll: aCollection [
	^ contents includesAll: aCollection
]

{ #category : #testing }
Array2D >> includesAny: aCollection [
	^ contents includesAny: aCollection
]

{ #category : #private }
Array2D >> indexForRow: row andColumn: column [
	(row between: 1 and: numberOfRows)
		ifFalse: [self error: '1st subscript out of range'].
	(column between: 1 and: numberOfColumns)
		ifFalse: [self error: '2nd subscript out of range'].
	^(row-1) * numberOfColumns + column
]

{ #category : #accessing }
Array2D >> indexOf: anElement [
	"If there are integers r, c such that (self at: r at: c) = anElement, answer some such r@c, otherwise answer 0@0. The order in which the receiver are searched is UNSPECIFIED except that it is the same as the order used by #indexOf:ifAbsent: and #readStream."

	^self indexOf: anElement ifAbsent: [0@0]
]

{ #category : #accessing }
Array2D >> indexOf: anElement ifAbsent: anExceptionBlock [
	"If there are integers r, c such that (self at: r at: c) = anElement, answer some such r@c, otherwise answer the result of anExceptionBlock."

	^self rowAndColumnForIndex:
		 (contents indexOf: anElement ifAbsent: [^ anExceptionBlock value])
]

{ #category : #enumerating }
Array2D >> indicesCollect: aBlock [

	| r i |
	r := Array new: numberOfRows * numberOfColumns.
	i := 0.
	1 to: numberOfRows do: [:row |
		1 to: numberOfColumns do: [:column |
			r at: (i := i+1) put: (aBlock value: row value: column)]].
	^ self class rows: numberOfRows columns: numberOfColumns contents: r
]

{ #category : #enumerating }
Array2D >> indicesDo: aBlock [
	1 to: numberOfRows do: [ :row |
		1 to: numberOfColumns do: [ :column |
			aBlock value: row value: column]]
]

{ #category : #enumerating }
Array2D >> indicesInject: start into: aBlock [

	|current|
	current := start.
	1 to: numberOfRows do: [ :row |
		1 to: numberOfColumns do: [ :column |
			current := aBlock value: current value: row value: column ] ].
	^ current
]

{ #category : #enumerating }
Array2D >> intersection: aCollection [
	"Union is in because the result is always a Set.
	 Difference and intersection are out because the result is like the receiver,
	 and with irregular seleection that cannot be."
	self shouldNotImplement
]

{ #category : #testing }
Array2D >> isSequenceable [
	"LIE so that arithmetic on matrices will work.
	 What matters for arithmetic is not that there should be random indexing
	 but that the structure should be stable and independent of the values of
	 the elements.  #isSequenceable is simply the wrong question to ask."
	^ true
]

{ #category : #accessing }
Array2D >> numberOfColumns [
	^ numberOfColumns
]

{ #category : #accessing }
Array2D >> numberOfColumns: anObject [
	numberOfColumns := anObject
]

{ #category : #accessing }
Array2D >> numberOfRows [
	^ numberOfRows
]

{ #category : #accessing }
Array2D >> numberOfRows: anObject [
	numberOfRows := anObject
]

{ #category : #testing }
Array2D >> occurrencesOf: anObject [
	^ contents occurrencesOf: anObject
]

{ #category : #copying }
Array2D >> postCopy [
	super postCopy.
	contents := contents copy
]

{ #category : #arithmetic }
Array2D >> preMultiplyByArray: a [
	"Answer a +* self where a is an Array."

	numberOfRows = 1 ifFalse: [self error: 'dimensions do not conform'].
	^Array2D rows: a size columns: numberOfColumns tabulate: [:row :col |
		(a at: row) * (contents at: col)]
]

{ #category : #arithmetic }
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

{ #category : #printing }
Array2D >> printOn: aStream [

	aStream nextPutAll: '('.
	(1 to: self numberOfRows)
		do: [ :r |
				(self atRow: r)
					do: [ :each | aStream print: each ]
					separatedBy: [ aStream space ]]
		separatedBy: [ aStream cr ].
	aStream nextPutAll: ' )'
]

{ #category : #private }
Array2D >> privateContents [
	"Only used in #, #,, and #= so far.
	 It used to be called #contents, but that clashes with Collection>>contents."

	^ contents
]

{ #category : #converting }
Array2D >> readStream [
	"Answer a ReadStream that returns all the elements of the receiver in some UNSPECIFIED order."
	^ contents readStream
]

{ #category : #enumerating }
Array2D >> reject: aBlock [
	self shouldNotImplement
]

{ #category : #removing }
Array2D >> remove: anObject ifAbsent: anExceptionBlock [
	self shouldNotImplement
]

{ #category : #removing }
Array2D >> removeAll [

	self shouldNotImplement
]

{ #category : #accessing }
Array2D >> replaceAll: oldObject with: newObject [
	"Replace all occurrences of oldObject with newObject in the receiver."
	contents replaceAll: oldObject with: newObject
]

{ #category : #private }
Array2D >> rowAndColumnForIndex: index [
	|t|

	t := index - 1.
	^(t // numberOfColumns + 1)@(t \\ numberOfColumns + 1)
]

{ #category : #accessing }
Array2D >> rowCount [
	^numberOfRows
]

{ #category : #private }
Array2D >> rows: rows columns: columns contents: anArray [
	[rows isInteger and: [rows >= 0]] assert.
	[columns isInteger and: [columns >= 0]] assert.
	[rows * columns = anArray size] assert.
	numberOfRows := rows.
	numberOfColumns := columns.
	contents := anArray.
	^self
]

{ #category : #enumerating }
Array2D >> select: aBlock [
	self shouldNotImplement
]

{ #category : #copying }
Array2D >> shuffled [
	^self class rows: numberOfRows columns: numberOfColumns contents: (contents shuffled)
]

{ #category : #copying }
Array2D >> shuffledBy: aRandom [
	^self class rows: numberOfRows columns: numberOfColumns contents: (contents copy shuffleBy: aRandom)
]

{ #category : #accessing }
Array2D >> size [
	^ contents size
]

{ #category : #printing }
Array2D >> storeOn: aStream [
	aStream nextPut: $(; nextPutAll: self class name;
		nextPutAll: ' rows: '; store: numberOfRows;
		nextPutAll: ' columns: '; store: numberOfColumns;
		nextPutAll: ' contents: '; store: contents;
		nextPut: $)
]

{ #category : #accessing }
Array2D >> swap: r1 at: c1 with: r2 at: c2 [
	contents swap: (self indexForRow: r1 andColumn: c1)
			 with: (self indexForRow: r2 andColumn: c2)
]

{ #category : #'row/column operations' }
Array2D >> swapColumn: anIndex withColumn: anotherIndex [
	|a b|

	a := self indexForRow: 1 andColumn: anIndex.
	b := self indexForRow: 1 andColumn: anotherIndex.
	numberOfRows timesRepeat: [
		contents swap: a with: b.
		a := a + numberOfColumns.
		b := b + numberOfColumns]
]

{ #category : #'row/column operations' }
Array2D >> swapRow: anIndex withRow: anotherIndex [
	| a b |
	a := self indexForRow: anIndex andColumn: 1.
	b := self indexForRow: anotherIndex andColumn: 1.
	numberOfColumns timesRepeat: [
		contents swap: a with: b.
		a := a + 1.
		b := b + 1]
]

{ #category : #'row/column operations' }
Array2D >> transposed [
	[numberOfRows = numberOfColumns] assert.
	^ self indicesCollect: [ :row :column | self at: column at: row ]
]

{ #category : #enumerating }
Array2D >> with: aCollection collect: aBlock [
	"aCollection must support #at:at: and be at least as large as the receiver."

	^self withIndicesCollect: [:each :row :column |
		aBlock value: each value: (aCollection at: row at: column)]
]

{ #category : #enumerating }
Array2D >> with: aCollection do: aBlock [
	"aCollection must support #at:at: and be at least as large as the receiver."

	self withIndicesDo: [:each :row :column |
		aBlock value: each value: (aCollection at: row at: column)]
]

{ #category : #enumerating }
Array2D >> with: aCollection inject: startingValue into: aBlock [
	"aCollection must support #at:at: and be at least as large as the receiver."

	^ self withIndicesInject: startingValue into: [:value :each :row :column |
			aBlock value: value value: each value: (aCollection at: row at: column)]
]

{ #category : #enumerating }
Array2D >> withIndicesCollect: aBlock [

	|i r|
	i := 0.
	r := contents shallowCopy.
	1 to: numberOfRows do: [ :row |
		1 to: numberOfColumns do: [ :column |
			i := i+1.
			r at: i put: (aBlock value: (r at: i) value: row value: column)]].
	^ self class rows: numberOfRows columns: numberOfColumns contents: r
]

{ #category : #enumerating }
Array2D >> withIndicesDo: aBlock [

	| i |
	i := 0.
	1 to: numberOfRows do: [:row |
		1 to: numberOfColumns do: [:column |
			aBlock value: (contents at: (i := i+1)) value: row value: column]]
]

{ #category : #enumerating }
Array2D >> withIndicesInject: start into: aBlock [

	| i current |
	i := 0.
	current := start.
	1 to: numberOfRows do: [ :row |
		1 to: numberOfColumns do: [ :column |
			current := aBlock
								value: current
								value: (contents at: (i := i+1))
								value: row value: column ] ].
	^ current
]
