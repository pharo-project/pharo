Extension { #name : #Metaclass }

{ #category : #'*CodeExport' }
Metaclass >> fileOutInitializerOn: aStream [
	(self includesSelector: #initialize)
		ifTrue: [ aStream newLine.
			aStream nextChunkPut: self soleInstance name , ' initialize' ]
]

{ #category : #'*CodeExport' }
Metaclass >> fileOutOn: aFileStream [

	^ self fileOutOn: aFileStream initializing: true
]

{ #category : #'*CodeExport' }
Metaclass >> fileOutOn: aFileStream initializing: aBool [
	super fileOutOn: aFileStream.
	(aBool and: [ self includesSelector: #initialize ])
		ifTrue: [ aFileStream newLine.
			aFileStream newLine.
			aFileStream nextChunkPut: thisClass name , ' initialize'.
			aFileStream newLine ]
]

{ #category : #'*CodeExport' }
Metaclass >> nonTrivial [ 
	"Answer whether the receiver has any methods or instance variables."

	^ self instVarNames notEmpty or: [self hasMethods or: [self hasTraitComposition]]
]
