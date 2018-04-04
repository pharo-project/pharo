Extension { #name : #AbstractFileReference }

{ #category : #'*GT-InspectorExtensions-Core' }
AbstractFileReference >> gtInspectorActionMaterializeFuel [
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
AbstractFileReference >> gtInspectorContentsIn: composite [
	<gtInspectorPresentationOrder: 5>
	composite text
			title: 'Contents';
			format: #asText;
			display: [ | maxBytes buffer atEnd stringContents displayStream displayString |
				maxBytes := 10000.
				self binaryReadStreamDo: [ :stream | 
					buffer := stream next: maxBytes.
					atEnd := stream atEnd ].
				displayString := [ 
					stringContents := ZnCharacterEncoder utf8 decodeBytes: buffer.
					atEnd ifTrue: 
						[ stringContents ]
					ifFalse:
						[ stringContents, '  ... truncated ...' ] ]
							on: Error 
							do: [ 
								displayStream := (String new: maxBytes * 5) writeStream.
								buffer hexDumpOn: displayStream max: maxBytes.
								displayString := displayStream contents asText.
								displayString addAttribute: (TextFontReference
									toFont: (LogicalFont 
										familyName: 'Source Code Pro' 
										pointSize: StandardFonts defaultFont pointSize)) ].
				displayString asText ];
			withLineNumbers: true;
			act: [ :text |
				self ensureDelete; writeStreamDo: [ :s | s nextPutAll: text text asString ] ]
				icon: GLMUIThemeExtraIcons glamorousSave
				on: $s
				entitled: 'Save';
			when: [ self isFile ]
]

{ #category : #'*GT-InspectorExtensions-Core' }
AbstractFileReference >> gtInspectorFuelIn: composite [
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
AbstractFileReference >> gtInspectorGifIn: composite [
	<gtInspectorPresentationOrder: 0>
	composite morph
			title: 'Picture';
			display: [ self binaryReadStreamDo: [ :stream | GIFReadWriter formFromStream: stream ] ];
			when: [ self isFile and: [self mimeTypes notNil and: [ self mimeTypes first matches: ZnMimeType imageGif ] ] ]
]

{ #category : #'*GT-InspectorExtensions-Core' }
AbstractFileReference >> gtInspectorItemsIn: composite [
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
AbstractFileReference >> gtInspectorJpegIn: composite [
	<gtInspectorPresentationOrder: 0>
	composite morph
			title: 'Picture';
			display: [ self binaryReadStreamDo: [ :stream | ImageReadWriter formFromStream: stream ] ];
			when: [ self isFile and: [self mimeTypes notNil and: [ self mimeTypes first matches: ZnMimeType imageJpeg ] ] ]
]

{ #category : #'*GT-InspectorExtensions-Core' }
AbstractFileReference >> gtInspectorPngIn: composite [
	<gtInspectorPresentationOrder: 0>
	composite morph
			title: 'Picture';
			display: [ self binaryReadStreamDo: [ :stream | PNGReadWriter formFromStream: stream ] ];
			when: [ self isFile and: [ self mimeTypes notNil and: [ self mimeTypes first matches: ZnMimeType imagePng ] ] ]
]

{ #category : #'*GT-InspectorExtensions-Core' }
AbstractFileReference >> gtInspectorStScriptIn: composite [
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
AbstractFileReference >> gtInspectorZipItemsIn: composite [
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
