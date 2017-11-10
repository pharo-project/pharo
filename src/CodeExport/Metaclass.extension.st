Extension { #name : #Metaclass }

{ #category : #'*CodeExport' }
Metaclass >> fileOutInitializerOn: aStream [
	(self includesSelector: #initialize) ifTrue: 
		[aStream cr.
		aStream nextChunkPut: self soleInstance name , ' initialize'].
]

{ #category : #'*CodeExport' }
Metaclass >> fileOutOn: aFileStream [

	<localMetaclassMethod> 
    "This method is supposed to be local in Metaclass because of a good reason.
    We use this pragma to test if Metaclass does not contain some accidental 
    local selectors."

	^ self fileOutOn: aFileStream initializing: true
]

{ #category : #'*CodeExport' }
Metaclass >> fileOutOn: aFileStream initializing: aBool [

	<localMetaclassMethod> 
    "This method is supposed to be local in Metaclass because of a good reason.
    We use this pragma to test if Metaclass does not contain some accidental 
    local selectors."

	super fileOutOn: aFileStream.
	(aBool and:[ self includesSelector: #initialize ]) ifTrue: [
		aFileStream cr.
		aFileStream cr.
		aFileStream nextChunkPut: thisClass name , ' initialize'.
		aFileStream cr ]
]

{ #category : #'*CodeExport' }
Metaclass >> nonTrivial [ 
	"Answer whether the receiver has any methods or instance variables."

	^ self instVarNames notEmpty or: [self hasMethods or: [self hasTraitComposition]]
]
