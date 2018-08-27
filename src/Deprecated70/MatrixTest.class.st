"
SUnit tests for matrix
"
Class {
	#name : #MatrixTest,
	#superclass : #TestCase,
	#instVars : [
		'matrix1',
		'matrix2',
		'matrix3',
		'matrix23'
	],
	#category : #Deprecated70
}

{ #category : #running }
MatrixTest >> setUp [
	super setUp.
	matrix1 := Array2D new: 2.
	matrix1 at:1 at:1 put: 1.
	matrix1 at:1 at:2 put: 3.
	matrix1 at:2 at:1 put: 2.
	matrix1 at:2 at:2 put: 4.
	
	matrix2 := Array2D new: 2.
	matrix2 at:1 at:1 put: 3.
	matrix2 at:1 at:2 put: 7.
	matrix2 at:2 at:1 put: 4.
	matrix2 at:2 at:2 put: 8.
	
	matrix23 := Array2D rows: 3 columns: 2.
	matrix23 at: 1 at: 1 put: 11.
	matrix23 at: 1 at: 2 put: 21.
	matrix23 at: 2 at: 1 put: 12.
	matrix23 at: 2 at: 2 put: 22.
	matrix23 at: 3 at: 1 put: 13.
	matrix23 at: 3 at: 2 put: 23.
]

{ #category : #'tests - accessing' }
MatrixTest >> testAtAllPut [

	| m | 
	m := Array2D new: 3. 
	m do: [ :each | self assert: each equals: nil ].
	m atAllPut: 1.
	m do: [ :each | self assert: each equals: 1 ].
]

{ #category : #'tests - accessing' }
MatrixTest >> testAtAt [
	self should:[matrix1 at: 2 at: 3] raise: Error.
	self should:[matrix1 at: 3 at: 2] raise: Error.
	self should:[matrix1 at: 3 at: 3] raise: Error.
	self should:[matrix1 at: 0 at: 1] raise: Error.
	self should:[matrix1 at: 1 at: 0] raise: Error.
	self should:[matrix1 at: 0 at: 0] raise: Error.
	
	self assert: (matrix1 at: 1 at: 1) equals: 1.
	self assert: (matrix23 at: 1 at: 1) equals: 11.
	self assert: (matrix23 at: 1 at: 2) equals: 21.
	self assert: (matrix23 at: 2 at: 1) equals: 12.
	self assert: (matrix23 at: 2 at: 2) equals: 22.
	self assert: (matrix23 at: 3 at: 1) equals: 13.
	self assert: (matrix23 at: 3 at: 2) equals: 23.
]

{ #category : #'tests - accessing' }
MatrixTest >> testAtColumn [

	self assert: (matrix23 atColumn: 1) equals: #(11 12 13).
	self assert: (matrix23 atColumn: 2) equals: #(21 22 23).
	
]

{ #category : #'tests - accessing' }
MatrixTest >> testAtRow [

	self assert: (matrix23 atRow: 1) equals: #(11 21).
	self assert: (matrix23 atRow: 2) equals: #(12 22).
	self assert: (matrix23 atRow: 3) equals: #(13 23).
]

{ #category : #'tests - accessing' }
MatrixTest >> testCollectCreatesANewMatrix [

	| m m2 | 
	m := Array2D new: 3. 
	m atAllPut: -1.
	self assert: (m occurrencesOf: -1) equals: 9.
	m2 := m collect: [ :each | each abs ].
	m2 collect: [ :each | self assert: each equals: 1 ].
	self assert: (m2 occurrencesOf: 1) equals: 9
]

{ #category : #'tests - instance creation' }
MatrixTest >> testColumnVector [

	| m m2 | 

	m := Array2D columnVector: #(1 2 3 4 5).

	self assert: m numberOfColumns equals: 1.
	self assert: m numberOfRows equals: 5.
	1 to: 5 do: [ :i |
		self assert: (m at: i at: 1) equals: i.].

	m2 := Array2D columnVector: Array new.

	self assert: m2 numberOfColumns equals: 1.
	self assert: m2 numberOfRows equals: 0.
	
]

{ #category : #'tests - accessing' }
MatrixTest >> testContentsEncodings [

	self assert: matrix23 privateContents equals: #(11 21 12 22 13 23)
]

{ #category : #'tests - copying' }
MatrixTest >> testCopy [

	| copyMatrix |
	copyMatrix := matrix1 copy.
	self assert: matrix1 = copyMatrix 
]

{ #category : #'tests - instance creation' }
MatrixTest >> testDiagonal [

	| m m2 | 

	m := Array2D diagonal: #(1 2 3 4 5).

	self assert: m numberOfColumns equals: 5.
	self assert: m numberOfRows equals: 5.
	self assert: (m occurrencesOf: 0) equals: 20.
	1 to: 5 do: [ :i |
		self assert: (m at: i at: i) equals: i.].

	m2 := Array2D diagonal: Array new.

	self assert: m2 numberOfColumns equals: 0.
	self assert: m2 numberOfRows equals: 0.
	
]

{ #category : #'tests - instance creation' }
MatrixTest >> testIdentity [

	| m  m2 | 
	m := Array2D new: 3 element: 0.
	m at: 1 at:1 put: 1.
	m at: 2 at:2 put: 1.
	m at: 3 at:3 put: 1.
	self assert: (m occurrencesOf: 1) equals: 3.
	m2 := Array2D identity: 3.
	self assert: (m2 = m).

]

{ #category : #'tests - instance creation' }
MatrixTest >> testIdentityOtherTest [

	| m | 
	m := Array2D identity: 3.
	self assert: (m at: 1 at:1) equals: 1.
	self assert: (m at: 2 at:2) equals: 1.
	self assert: (m at: 3 at:3) equals: 1.
	self assert: (m occurrencesOf: 1) equals: 3.

]

{ #category : #'tests - testing' }
MatrixTest >> testIncludes [
	self assert:
			((1 to: 4)
				allSatisfy: [:i | matrix1 includes: i])

]

{ #category : #'tests - arithmetic' }
MatrixTest >> testMultiply [
	
	| result |
	self	should: [matrix1 preMultiplyByMatrix: (Array2D new: 3)] raise: Error.

	result := matrix2 preMultiplyByMatrix: matrix1.
	self assert: (result at: 1 at: 1) = 15.
	self assert: (result at: 1 at: 2) = 31.
	self assert: (result at: 2 at: 1) = 22.
	self assert: (result at: 2 at: 2) = 46
]

{ #category : #'tests - instance creation' }
MatrixTest >> testNewTabulate [

	| m m2 | 

	m := Array2D new: 5 tabulate: [ :a :b | a*b ].

	self assert: m numberOfColumns equals: 5.
	self assert: m numberOfRows equals: 5.
	self assert: (m occurrencesOf: 8) equals: 2.
	1 to: 5 do: [ :i |
		self assert: (m at: i at: 1) equals: i.
		self assert: (m at: 1 at: i) equals: i.
		self assert: (m at: i at: i) equals: i*i.].

	m2 := Array2D new: 0 tabulate: [ :a :b | a*b ].
	
	self assert: m2 numberOfColumns equals: 0.
	self assert: m2 numberOfRows equals: 0.
	self assert: (m2 occurrencesOf: 1) equals: 0.
	
]

{ #category : #'tests - instance creation' }
MatrixTest >> testOnes [

	| m m2 | 

	m := Array2D ones: 10.

	self assert: m numberOfColumns equals: 10.
	self assert: m numberOfRows equals: 10.
	self assert: (m occurrencesOf: 1) equals: (10*10).

	m2 := Array2D ones: 0.

	self assert: m2 numberOfColumns equals: 0.
	self assert: m2 numberOfRows equals: 0.
	self assert: (m2 occurrencesOf: 1) equals: 0.
	
]

{ #category : #'tests - accessing' }
MatrixTest >> testReplaceAll [

	matrix1 replaceAll: 1 with: 10.
	self assert: (matrix1 at: 1 at: 1) equals: 10.
	self assert: (matrix1 at: 2 at: 1) equals: 2.
	self assert: (matrix1 at: 1 at: 2) equals: 3.
	self assert: (matrix1 at: 2 at: 2) equals: 4.
]

{ #category : #'tests - instance creation' }
MatrixTest >> testRowVector [

	| m m2 | 

	m := Array2D rowVector: #(1 2 3 4 5).

	self assert: m numberOfColumns equals: 5.
	self assert: m numberOfRows equals: 1.
	1 to: 5 do: [ :i |
		self assert: (m at: 1 at: i) equals: i.].

	m2 := Array2D rowVector: Array new.

	self assert: m2 numberOfColumns equals: 0.
	self assert: m2 numberOfRows equals: 1.
	
]

{ #category : #'tests - accessing' }
MatrixTest >> testSquaredWithAllElement [

	self assert: ((Array2D new: 3 element: -1) occurrencesOf: -1) equals: 9.

]

{ #category : #'tests - accessing' }
MatrixTest >> testSwap [
	matrix1 swap: 1 at: 2 with: 1 at: 1.
	self assert: (matrix1 at: 1 at: 1) = 3.
	self assert: (matrix1 at: 1 at: 2) = 1.
]

{ #category : #'tests - testing' }
MatrixTest >> testTabulate [
	| m |
	m := Array2D rows: 3 columns: 2 tabulate: [ :row :column | column * 10 + row ].
	self assert: (m at: 1 at: 1) equals: 11.
	self assert: (m at: 1 at: 2) equals: 21.
	self assert: (m at: 2 at: 1) equals: 12.
	self assert: (m at: 2 at: 2) equals: 22.
	self assert: (m at: 3 at: 1) equals: 13.
	self assert: (m at: 3 at: 2) equals: 23.
]

{ #category : #'tests - testing' }
MatrixTest >> testTabulateEquals [
	| m |
	m := Array2D rows: 3 columns: 2 tabulate: [ :row :column | column * 10 + row ].
	self assert: (m = matrix23)

]

{ #category : #'tests - accessing' }
MatrixTest >> testTransposed [
	| transposedMatrix |
	transposedMatrix := matrix1 transposed.
	self assert: (transposedMatrix at: 1 at: 1) equals: 1.
	self assert: (transposedMatrix at: 1 at: 2) equals: 2.
	self assert: (transposedMatrix at: 2 at: 1) equals: 3.
	self assert: (transposedMatrix at: 2 at: 2) equals: 4
]
