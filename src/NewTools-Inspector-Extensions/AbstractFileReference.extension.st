Extension { #name : #AbstractFileReference }

{ #category : #'*NewTools-Inspector-Extensions' }
AbstractFileReference >> inspectorCompressedItems [
	<inspectorPresentationOrder: 0
		title: 'Compressed Items' 
		if: #isCompressedFile>

	| items children root |
	root := (FileSystem zip: self) open workingDirectory.
	children := root directories, root files.
	items := self isRoot 
		ifTrue: [ children ]
		ifFalse: [ 
			children asOrderedCollection 
				addFirst: self parent;
				yourself ].

	^ SpTablePresenter new
		items: items;
		addColumn: (SpCompositeTableColumn new
			title: 'Name';
			addColumn: (SpImageTableColumn evaluated: [ :each | 
				self iconNamed: (each isDirectory 
					ifTrue: [ #glamorousFolder ] 
					ifFalse: [ #glamorousBrowse ]) ]);
			addColumn: (SpStringTableColumn evaluated: [ :each | 
					self parent = each 
						ifTrue: [ '..' ] 
						ifFalse: [ each basename ] ]));
		addColumn: (SpStringTableColumn 
			title: 'Size' 
			evaluated: [ :each | each humanReadableSize ]);
		addColumn: (SpStringTableColumn 
			title: 'Creation' 
			evaluated: [ :each | 
				String streamContents: [ :stream | 
					each creationTime printYMDOn: stream.
					stream nextPut: Character space.
					each creationTime printHMSOn: stream ] ])
]

{ #category : #'*NewTools-Inspector-Extensions' }
AbstractFileReference >> inspectorContents [
	<inspectorPresentationOrder: 0 
		title: 'Contents' 
		if: #isFile> 
	| maxBytes buffer atEnd stringContents displayStream displayString |
	
	maxBytes := 10000.
	
	self binaryReadStreamDo: [ :stream | 
		buffer := stream next: maxBytes.
		atEnd := stream atEnd ].
				
	displayString := [ 
			stringContents := ZnCharacterEncoder utf8 decodeBytes: buffer.
			atEnd 
				ifTrue: [ stringContents ]
				ifFalse: [ stringContents, '  ... truncated ...' ] ]
		on: Error 
		do: [ 
			displayStream := (String new: maxBytes * 5) writeStream.
			buffer hexDumpOn: displayStream max: maxBytes.
			displayString := displayStream contents ].
	
	^ SpCodePresenter new
		withoutSyntaxHighlight;
		text: displayString;
		yourself
]

{ #category : #'*NewTools-Inspector-Extensions' }
AbstractFileReference >> inspectorFuel [
	<inspectorPresentationOrder: 40
		title: 'Fuel Header' 
		if: #isFuel>
	
	^ SpTablePresenter new
		items: (FLMaterializer materializeHeaderFromFileNamed: self fullName) stFuelMetaData;
		addColumn: (SpStringTableColumn title: 'Property' evaluated: [ :assoc | assoc key ]);
		addColumn: (SpStringTableColumn title: 'Value' evaluated: [ :assoc | assoc value ]);	
		yourself
]

{ #category : #'*NewTools-Inspector-Extensions' }
AbstractFileReference >> inspectorGif [
	<inspectorPresentationOrder: 0
		title: 'Picture' 
		if: #isImageGif>
	
	^ SpMorphPresenter new
		morph: (self binaryReadStreamDo: [ :stream | GIFReadWriter formFromStream: stream ]);
		yourself
]

{ #category : #'*NewTools-Inspector-Extensions' }
AbstractFileReference >> inspectorItems [
	<inspectorPresentationOrder: 0 
		title: 'Items' 
		if: #isDirectory>
	| items |
	
	items := 	self directories, self files.
	self isRoot ifFalse: [ 
		items := items copyWithFirst: self parent ]. 

	^ SpTablePresenter new 
		items: items;
		addColumn: (SpCompositeTableColumn new 
			title: 'Name';
			addColumn: (SpImageTableColumn evaluated: [ :each |
				each isDirectory 
					ifTrue: [ self iconNamed: #glamorousFolder ] 
					ifFalse: [ self iconNamed: #glamorousBrowse ] ]) beNotExpandable;
			addColumn: (SpStringTableColumn evaluated: [ :each | 
				(self isChildOf: each) 
					ifTrue: [ '..' ] 
					ifFalse: [ each basename ]]);
			yourself);
		addColumn: (SpStringTableColumn 
			title: 'Size'
			evaluated: [ :each | 
				[ each humanReadableSize ]
					on: Error 
					do: [ 'N/A' translated ] ]);
		addColumn: (SpStringTableColumn 
			title: 'Creation'
			evaluated: [ :each | 
				[ String streamContents: [ :s | 
					each creationTime printYMDOn: s.
					s nextPut: Character space.
					each creationTime printHMSOn: s ]]
				on: Error 
				do: [ 'N/A' translated  ] ]);
		yourself
]

{ #category : #'*NewTools-Inspector-Extensions' }
AbstractFileReference >> inspectorJpeg [
	<inspectorPresentationOrder: 0
		title: 'Picture' 
		if: #isImageJpeg>
	
	^ SpMorphPresenter new
		morph: (self binaryReadStreamDo: [ :stream | ImageReadWriter formFromStream: stream ]);
		yourself
]

{ #category : #'*NewTools-Inspector-Extensions' }
AbstractFileReference >> inspectorPng [
	<inspectorPresentationOrder: 0
		title: 'Picture' 
		if: #isImagePng>
	
	^ SpMorphPresenter new
		morph: (self binaryReadStreamDo: [ :stream | PNGReadWriter formFromStream: stream ]);
		yourself
]

{ #category : #'*NewTools-Inspector-Extensions' }
AbstractFileReference >> inspectorStScript [
	<inspectorPresentationOrder: 0 
		title: 'Script' 
		if: #isScript>
	
	^ SpCodePresenter new 
		text: (self readStreamDo: #contents);
		yourself
]

{ #category : #'*NewTools-Inspector-Extensions' }
AbstractFileReference >> isCompressedFile [

	^ #('zip' 'jar' 'ear' 'war' 'mcz') includes: self extension
]

{ #category : #'*NewTools-Inspector-Extensions' }
AbstractFileReference >> isFuel [

	^ self isFile and: [ self extension = 'fuel' ]
]

{ #category : #'*NewTools-Inspector-Extensions' }
AbstractFileReference >> isImageGif [

	^ self isFile 
		and: [ self mimeTypes notNil 
		and: [ self mimeTypes first matches: ZnMimeType imageGif ] ]
]

{ #category : #'*NewTools-Inspector-Extensions' }
AbstractFileReference >> isImageJpeg [

	^ self isFile 
		and: [ self mimeTypes notNil 
		and: [ self mimeTypes first matches: ZnMimeType imageJpeg ] ]
]

{ #category : #'*NewTools-Inspector-Extensions' }
AbstractFileReference >> isImagePng [

	^ self isFile 
		and: [ self mimeTypes notNil 
		and: [ self mimeTypes first matches: ZnMimeType imagePng ] ]
]

{ #category : #'*NewTools-Inspector-Extensions' }
AbstractFileReference >> isScript [
	
	^ self isFile and: [ self extension = 'st' ]
]
