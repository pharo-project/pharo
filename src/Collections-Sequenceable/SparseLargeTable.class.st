"
Derivated from Stephan Pair's LargeArray, but to hold a sparse table, in which most of the entries are the same default value, it uses some tricks.
"
Class {
	#name : #SparseLargeTable,
	#superclass : #ArrayedCollection,
	#type : #variable,
	#instVars : [
		'base',
		'size',
		'chunkSize',
		'defaultValue'
	],
	#category : #'Collections-Sequenceable-Sparse'
}

{ #category : #accessing }
SparseLargeTable class >> defaultChunkSize [

	^100
]

{ #category : #accessing }
SparseLargeTable class >> defaultChunkSizeForFiles [

	^8000
]

{ #category : #'instance creation' }
SparseLargeTable class >> new: size [

	^self new: size chunkSize: self defaultChunkSize
]

{ #category : #'instance creation' }
SparseLargeTable class >> new: size chunkSize: chunkSize [

	^self new: size chunkSize: chunkSize arrayClass: Array
]

{ #category : #'instance creation' }
SparseLargeTable class >> new: size chunkSize: chunkSize arrayClass: aClass [

	^self new: size chunkSize: chunkSize arrayClass: Array base: 1
]

{ #category : #'instance creation' }
SparseLargeTable class >> new: size chunkSize: chunkSize arrayClass: aClass base: b [

	^self new: size chunkSize: chunkSize arrayClass: Array base: 1 defaultValue: nil
]

{ #category : #'instance creation' }
SparseLargeTable class >> new: size chunkSize: chunkSize arrayClass: aClass base: b defaultValue: d [

	| basicSize |
	(basicSize := ((size - 1) // chunkSize) + 1) = 0
		ifTrue: [basicSize := 1].
	^(self basicNew: basicSize)
		initChunkSize: chunkSize size: size arrayClass: aClass base: b defaultValue: d;
		yourself
]

{ #category : #private }
SparseLargeTable >> allDefaultValueSubtableAt: index [

	| t |
	t := self basicAt: index.
	t ifNil: [^ true].
	t do: [:e |
		e ~= defaultValue ifTrue: [^ false].
	].
	^ true
]

{ #category : #private }
SparseLargeTable >> analyzeSpaceSaving [

	| total elems tablesTotal nonNilTables |
	total := size - base + 1.
	elems := 0.
	base to: size do: [:i | (self at: i) ~= defaultValue ifTrue: [elems := elems + 1]].
	tablesTotal := self basicSize.
	nonNilTables := 0.
	1 to: self basicSize do: [:i | (self basicAt: i) ifNotNil: [nonNilTables := nonNilTables + 1]].

	^ String streamContents: [:strm |
		strm nextPutAll: 'total: '.
		strm nextPutAll: total printString.
		strm nextPutAll: ' elements: '.
		strm nextPutAll: elems printString.
		strm nextPutAll: ' tables: '.
		strm nextPutAll: tablesTotal printString.
		strm nextPutAll: ' non-nil: '.
		strm nextPutAll: nonNilTables printString.
	]
]

{ #category : #accessing }
SparseLargeTable >> arrayClass [

	^(self basicAt: 1) class
]

{ #category : #accessing }
SparseLargeTable >> at: index [

	self pvtCheckIndex: index.
	^self noCheckAt: index
]

{ #category : #accessing }
SparseLargeTable >> at: index put: value [

	self pvtCheckIndex: index.
	^self noCheckAt: index put: value
]

{ #category : #accessing }
SparseLargeTable >> base [

	^ base
]

{ #category : #accessing }
SparseLargeTable >> chunkSize [

	^chunkSize
]

{ #category : #copying }
SparseLargeTable >> copyEmpty [
	"Answer a copy of the receiver that contains no elements."
	^self speciesNew: 0
]

{ #category : #initialization }
SparseLargeTable >> initChunkSize: aChunkSize size: aSize arrayClass: aClass base: b defaultValue: d [

	| lastChunkSize |
	chunkSize := aChunkSize.
	size := aSize.
	base := b.
	defaultValue := d.
	1 to: (self basicSize - 1) do: [ :in | self basicAt: in put: (aClass new: chunkSize withAll: defaultValue) ].
	lastChunkSize := size \\ chunkSize.
	lastChunkSize = 0 ifTrue: [lastChunkSize := chunkSize].
	size = 0
		ifTrue: [self basicAt: 1 put: (aClass new: 0)]
		ifFalse: [self basicAt: self basicSize put: (aClass new: lastChunkSize withAll: defaultValue)]
]

{ #category : #accessing }
SparseLargeTable >> noCheckAt: index [
	| chunkIndex t |

	chunkIndex := index - base // chunkSize + 1.
	(chunkIndex > self basicSize or: [chunkIndex < 1]) ifTrue: [^ defaultValue].
	t := self basicAt: chunkIndex.
	t ifNil: [^ defaultValue].
	^ t at: (index - base + 1 - (chunkIndex - 1 * chunkSize))
]

{ #category : #accessing }
SparseLargeTable >> noCheckAt: index put: value [

	| chunkIndex t |
	chunkIndex := index - base // chunkSize + 1.
	chunkIndex > self basicSize ifTrue: [^ value].
	t :=  self basicAt: chunkIndex.
	t ifNil: [^ value].
	^ t at: (index - base + 1 - (chunkIndex - 1 * chunkSize)) put: value
]

{ #category : #copying }
SparseLargeTable >> postCopy [
	super postCopy.
	1 to: self basicSize do: [:i | self basicAt: i put: (self basicAt: i) copy]
]

{ #category : #printing }
SparseLargeTable >> printElementsOn: aStream [
	| element |
	aStream nextPut: $(.
	base to: size do: [:index | element := self at: index. aStream print: element; space].
	self isEmpty ifFalse: [aStream skip: -1].
	aStream nextPut: $)
]

{ #category : #printing }
SparseLargeTable >> printOn: aStream [

	(#(String) includes: self arrayClass name)
		ifTrue: [^self storeOn: aStream].
	^super printOn: aStream
]

{ #category : #private }
SparseLargeTable >> privateSize: s [

	size := s
]

{ #category : #private }
SparseLargeTable >> pvtCheckIndex: index [

	index isInteger ifFalse: [self errorNonIntegerIndex].
	index < 1 ifTrue: [self errorSubscriptBounds: index].
	index > size ifTrue: [self errorSubscriptBounds: index]
]

{ #category : #private }
SparseLargeTable >> similarInstance [

	^self class
		new: self size
		chunkSize: self chunkSize
		arrayClass: self arrayClass
]

{ #category : #private }
SparseLargeTable >> similarInstance: newSize [

	^self class
		new: newSize
		chunkSize: self chunkSize
		arrayClass: self arrayClass
]

{ #category : #private }
SparseLargeTable >> similarSpeciesInstance [

	^self similarInstance
]

{ #category : #private }
SparseLargeTable >> similarSpeciesInstance: newSize [

	^self similarInstance: newSize
]

{ #category : #accessing }
SparseLargeTable >> size [

	^size
]

{ #category : #private }
SparseLargeTable >> speciesNew [

	^self species
		new: self size
		chunkSize: self chunkSize
		arrayClass: self arrayClass
]

{ #category : #private }
SparseLargeTable >> speciesNew: newSize [

	^self species
		new: newSize
		chunkSize: self chunkSize
		arrayClass: self arrayClass
]

{ #category : #printing }
SparseLargeTable >> storeOn: aStream [

	| x |
	(#(String) includes: self arrayClass name) ifTrue:
		[aStream nextPut: $'.
		1 to: self size do:
			[:i |
			aStream nextPut: (x := self at: i).
			x == $' ifTrue: [aStream nextPut: x]].
		aStream nextPutAll: ''' asLargeArrayChunkSize: '.
		aStream nextPutAll: self chunkSize asString.
		^self].
	^super storeOn: aStream
]

{ #category : #accessing }
SparseLargeTable >> zapDefaultOnlyEntries [

	1 to: self basicSize do: [:i |
		(self allDefaultValueSubtableAt: i) ifTrue: [self basicAt: i put: nil].
	]
]
