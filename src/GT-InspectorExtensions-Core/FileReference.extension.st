Extension { #name : #FileReference }

{ #category : #'*GT-InspectorExtensions-Core' }
FileReference >> gtDisplayOn: stream [
	filesystem printPath: path on: stream
]

{ #category : #'*GT-InspectorExtensions-Core' }
FileReference >> gtInspectorActionMaterializeFuel [
	<gtInspectorAction>
	^ GLMGenericAction new
		action: [ :composite | 
			| result |
			result := FLMaterializer materializeFromFileNamed: self fullName.
			composite selection: result ];
		icon: GLMUIThemeExtraIcons glamorousAdd;
		condition: [ self isFile and: [self extension = 'fuel' ] ];
		title: 'Materialize'
]

{ #category : #'*GT-InspectorExtensions-Core' }
FileReference >> gtInspectorContentsIn: composite [
	<gtInspectorPresentationOrder: 5>
	composite text
			title: 'Contents';
			display: [ 
				self readStreamDo: [ :stream | 
					| result |
					result := [(stream next: 10000) asString]
						on: Error 
						do: [ (stream binary next: 10000) asString ].
					stream size > 10000
						ifTrue: [ result, '  ... truncated ...' ]
						ifFalse: [ result ] ] ];
			withLineNumbers: true;
			act: [ :text |
				self ensureDelete; writeStreamDo: [ :s | s nextPutAll: text text asString ] ]
				icon: GLMUIThemeExtraIcons glamorousSave
				on: $s
				entitled: 'Save';
			when: [ self isFile ]
]

{ #category : #'*GT-InspectorExtensions-Core' }
FileReference >> gtInspectorFuelIn: composite [
	<gtInspectorPresentationOrder: 40>
	"(FileSystem disk workingDirectory / 'GTtest.fuel') writeStreamDo: [ :stream |
	FLSerializer newDefault
		at: #author putAdditionalObject: 'Author: Max Leske';
		at: #date putAdditionalObject: DateAndTime now;
		addPreMaterializationAction: [ Smalltalk inform: 'starting materialization' ];
		addPostMaterializationAction: [ :materialization | materialization inspect ];
		serialize: 'foo' on: stream binary ].
	
FLMaterializer materializeFromFileNamed: 'GTtest.fuel'"
	composite table
			title: 'Fuel Header';
			display: [
				(FLMaterializer materializeHeaderFromFileNamed: self fullName) gtGetMetaData ];
			column: 'Property' evaluated: [:assoc| assoc key ];
			column: 'Value' evaluated: [:assoc| assoc value];
			when: [ self isFile and: [self extension = 'fuel' ] ]
]

{ #category : #'*GT-InspectorExtensions-Core' }
FileReference >> gtInspectorGifIn: composite [
	<gtInspectorPresentationOrder: 0>
	composite morph
			title: 'Picture';
			display: [ self binaryReadStreamDo: [ :stream | GIFReadWriter formFromStream: stream ] ];
			when: [ self isFile and: [self mimeTypes notNil and: [ self mimeTypes first matches: ZnMimeType imageGif ] ] ]
]

{ #category : #'*GT-InspectorExtensions-Core' }
FileReference >> gtInspectorItemsIn: composite [
	<gtInspectorPresentationOrder: 0>
	^ composite fastTable 
		title: 'Items';
		display: [ 
			| children |
			children := self directories, self files.
			self isRoot 
				ifTrue: [ children ]
				ifFalse: [ 
					children asOrderedCollection 
						addFirst: self parent;
						yourself ] ];
		column: 'Name' evaluated: [:each | (self isChildOf: each) 
								ifTrue: [ '..' ] 
								ifFalse: [ each basename ]] width: 400;
		column: 'Size' evaluated: [:each | each humanReadableSize] width: 100;
		column: 'Creation' evaluated: [ :each | 
			String streamContents: [:s | 
				each creationTime printYMDOn: s.
				s nextPut: Character space.
				each creationTime printHMSOn: s ]];
		icon: [ :each | 
			each isDirectory 
				ifTrue: [ GLMUIThemeExtraIcons glamorousFolder ] 
				ifFalse: [ GLMUIThemeExtraIcons glamorousBrowse ] ];
		when: [self isDirectory and: [self hasChildren]];
		selectionAct: [:table | table selection delete. table update] entitled: 'Delete';
		act: [ :table |
			 (UIManager default request: 'New Directory name?' initialAnswer: '') 
					ifNotNil: [:name | (self / name) ensureCreateDirectory ].
			 table update
			 ] icon: GLMUIThemeExtraIcons glamorousFolder entitled: 'Create Directory';		
		act: [ :table |
			 (UIManager default request: 'New File name?' initialAnswer: '') 
					ifNotNil: [:name | (self / name) ensureCreateFile ].
			 table update
			 ] icon: GLMUIThemeExtraIcons glamorousBrowse entitled: 'Create File';		
		act: [ :table | table update ] icon: GLMUIThemeExtraIcons glamorousRefresh entitled: 'Refresh'
]

{ #category : #'*GT-InspectorExtensions-Core' }
FileReference >> gtInspectorJpegIn: composite [
	<gtInspectorPresentationOrder: 0>
	composite morph
			title: 'Picture';
			display: [ self binaryReadStreamDo: [ :stream | ImageReadWriter formFromStream: stream ] ];
			when: [ self isFile and: [self mimeTypes notNil and: [ self mimeTypes first matches: ZnMimeType imageJpeg ] ] ]
]

{ #category : #'*GT-InspectorExtensions-Core' }
FileReference >> gtInspectorPngIn: composite [
	<gtInspectorPresentationOrder: 0>
	composite morph
			title: 'Picture';
			display: [ self binaryReadStreamDo: [ :stream | PNGReadWriter formFromStream: stream ] ];
			when: [ self isFile and: [ self mimeTypes notNil and: [ self mimeTypes first matches: ZnMimeType imagePng ] ] ]
]

{ #category : #'*GT-InspectorExtensions-Core' }
FileReference >> gtInspectorStScriptIn: composite [
	<gtInspectorPresentationOrder: 0>
	composite pharoScript
			title: 'Script';
			display: [ self readStreamDo: [ :s | s contents ] ];
			act: [ :text |
					text selectionInterval: (1 to: text text size).
					text highlightEvaluateAndDo: [ :result | text selection: result ] ]
				icon: GLMUIThemeExtraIcons glamorousGo
				on: $G
				entitled: 'Do it all and go';
			installDefaultSelectionActions;
			act: [ :text |
				self ensureDelete; writeStreamDo: [ :s | s nextPutAll: text text asString ] ]
				icon: GLMUIThemeExtraIcons glamorousSave
				on: $s
				entitled: 'Save';
			when: [ self isFile and: [ self extension = 'st' or: [self extension = 'ph'] ] ]
]

{ #category : #'*GT-InspectorExtensions-Core' }
FileReference >> gtInspectorZipItemsIn: composite [
	<gtInspectorPresentationOrder: 0>
	^ composite table 
		title: 'Items';
		display: [ 
			| children root |
			root := (FileSystem zip: self) open workingDirectory.
			children := root directories, root files.
			self isRoot 
				ifTrue: [ children ]
				ifFalse: [ 
					children asOrderedCollection 
						addFirst: self parent;
						yourself ] ];
		column: 'Name' evaluated: [:each | self parent = each 
								ifTrue: [ '..' ] 
								ifFalse: [ each basename ]] width: 400;
		column: 'Size' evaluated: [:each | each humanReadableSize] width: 100;
		column: 'Creation' evaluated: [ :each | 
			String streamContents: [:s | 
				each creationTime printYMDOn: s.
				s nextPut: Character space.
				each creationTime printHMSOn: s ]];
		icon: [ :each | 
			each isDirectory 
				ifTrue: [ GLMUIThemeExtraIcons glamorousFolder ] 
				ifFalse: [ GLMUIThemeExtraIcons glamorousBrowse ] ];
		when: [ self isFile and: [ #('zip' 'jar' 'ear' 'war' 'mcz') includes: self extension ] ];
		act: [ :table | table update ] icon: GLMUIThemeExtraIcons glamorousRefresh entitled: 'Refresh'
]
