"
A version of SparseLargeTable that does not populate its bins until a value other than the default is stored.!
"
Class {
	#name : #SparseLargeArray,
	#superclass : #SparseLargeTable,
	#type : #variable,
	#instVars : [
		'arrayClass'
	],
	#category : #'Collections-Sequenceable-Sparse'
}

{ #category : #private }
SparseLargeArray >> analyzeSpaceSaving [

      | elems tablesTotal nonNilTables lastPage lastChunkSize |
      elems := 0.
       tablesTotal := self basicSize.
       nonNilTables := 0.
       lastPage := self basicAt: self basicSize.
       (lastChunkSize := size \\ chunkSize) = 0 ifTrue:
               [lastChunkSize := chunkSize].
       1 to: self basicSize do:
               [:i | | page |
               (page := self basicAt: i) ifNotNil:
                       [nonNilTables := nonNilTables + 1.
                        1 to: (page == lastPage ifTrue: [lastChunkSize] ifFalse: [chunkSize]) do:
                               [:j|
                               (page at: j) ~= defaultValue ifTrue:
                                       [elems := elems + 1]]]].

       ^String streamContents:
               [:strm |
               strm nextPutAll: 'total: '; print: size.
               strm nextPutAll: ' elements: '; print: elems.
               strm nextPutAll: ' tables: '; print: tablesTotal.
               strm nextPutAll: ' non-nil: '; print: nonNilTables]
]

{ #category : #accessing }
SparseLargeArray >> atAllPut: anObject [
       "Put anObject at every one of the receiver's indices."

       1 to: self basicSize do:
               [:i|
               self basicAt: i put: nil].
       defaultValue := anObject
]

{ #category : #initialization }
SparseLargeArray >> initChunkSize: aChunkSize size: aSize arrayClass: aClass base: b defaultValue: d [
       chunkSize := aChunkSize.
       size := aSize.
       base := b.
       defaultValue := d.
       arrayClass := aClass
]

{ #category : #accessing }
SparseLargeArray >> noCheckAt: index [
      ^(self basicAt: index - base // chunkSize + 1)
               ifNil: [defaultValue]
               ifNotNil: [:chunk| chunk at: index - base \\ chunkSize + 1]
]

{ #category : #accessing }
SparseLargeArray >> noCheckAt: index put: value [
       | chunkIndex chunk lastChunkSize |
       chunkIndex := index - base // chunkSize + 1.
       (chunk := self basicAt: chunkIndex) ifNil:
               [value = defaultValue ifTrue:
                       [^value].
               chunk := arrayClass
                                       new: ((chunkIndex == self basicSize
                                                  and: [(lastChunkSize := size \\ chunkSize) > 0])
                                                       ifTrue: [lastChunkSize]
                                                       ifFalse: [chunkSize])
                                       withAll: defaultValue.
               self basicAt: chunkIndex put: chunk].
       ^chunk at: index - base \\ chunkSize + 1 put: value
]
