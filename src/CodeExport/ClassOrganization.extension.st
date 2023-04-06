Extension { #name : #ClassOrganization }

{ #category : #'*CodeExport' }
ClassOrganization >> fileOutCommentOn: aFileStream [
	"Copy the class comment to aFileStream."

	self classComment ifNil: [ ^ self ].
	aFileStream
		cr;
		nextChunkPut: self classComment string
]

{ #category : #'*CodeExport' }
ClassOrganization >> putCommentOnFile: aFileStream forClass: aClass [
	"Store the comment about the class onto file, aFileStream."

	| header |
	self classComment isEmptyOrNil ifTrue:[ ^ self ].

	aFileStream cr; nextPut: $!.
	header := String streamContents: [:strm |
			strm nextPutAll: aClass name;
			nextPutAll: ' commentStamp: '.
			self commentStamp storeOn: strm.
			strm nextPutAll: ' prior: '; nextPutAll: '0'].
	aFileStream nextChunkPut: header.
	aClass organization fileOutCommentOn: aFileStream.
	aFileStream cr
]

{ #category : #'*CodeExport' }
ClassOrganization >> stringForFileOut [

	^ String streamContents: [ :aStream |
		  self protocols do: [ :p |
			  aStream
				  nextPut: $(;
				  nextPutAll: p name printString.
			  p methodSelectors do: [ :m |
				  aStream
					  space;
					  nextPutAll: m ].
			  aStream
				  nextPut: $);
				  cr ] ]
]
