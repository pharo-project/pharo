Extension { #name : #FileReference }

{ #category : #'*GT-SpotterExtensions-Core' }
FileReference >> isZipFile [
	^ self isFile and: [ #('zip' 'jar' 'ear' 'war') includes: self extension ]
]

{ #category : #'*GT-SpotterExtensions-Core' }
FileReference >> spotterForDirectoriesFor: aStep [
	<spotterOrder: 10>
	^ self isDirectory ifTrue: [
		aStep listProcessor
			allCandidates: [ self directories ];
			title: 'Directories';
			candidatesLimit: Float infinity;
			itemName: #basename;
			itemIcon: [ GLMUIThemeExtraIcons glamorousFolder ];
			filter: GTFilterSubstring ]
]

{ #category : #'*GT-SpotterExtensions-Core' }
FileReference >> spotterForFilesFor: aStep [
	<spotterOrder: 20>
	^ self isDirectory ifTrue: [
		aStep listProcessor
			allCandidates: [ self files ];
			title: 'Files';
			candidatesLimit: Float infinity;
			itemName: #basename;
			itemIcon: [ GLMUIThemeExtraIcons glamorousBrowse ];
			filter: GTFilterSubstring ]
]

{ #category : #'*GT-SpotterExtensions-Core' }
FileReference >> spotterForZipDirectoriesFor: aStep [
	<spotterOrder: 10>
	| zipFileSystem |
	self isZipFile ifFalse: [ ^ self ].
	aStep listProcessor
			allCandidates: [ 
				zipFileSystem := (FileSystem zip: self) open.
				zipFileSystem workingDirectory directories ];
			title: 'Directories';
			candidatesLimit: Float infinity;
			itemName: #basename;
			itemIcon: [ GLMUIThemeExtraIcons glamorousFolder ];
			filter: GTFilterSubstring
]

{ #category : #'*GT-SpotterExtensions-Core' }
FileReference >> spotterForZipFilesFor: aStep [
	<spotterOrder: 10>
	| zipFileSystem |
	self isZipFile ifFalse: [ ^ self ].
	aStep listProcessor
			allCandidates: [ 
				zipFileSystem := (FileSystem zip: self) open.
				zipFileSystem workingDirectory files ];
			title: 'Files';
			candidatesLimit: Float infinity;
			itemName: #basename;
			itemIcon: [ GLMUIThemeExtraIcons glamorousBrowse ];
			filter: GTFilterSubstring
	
]

{ #category : #'*GT-SpotterExtensions-Core' }
FileReference >> spotterPreviewContentsIn: aComposite [
	<spotterPreview: 20>
	
	self isFile ifFalse: [ ^ self ].
	aComposite text
		title: [ self gtDisplayString ]; 
		display: #yourself;
		entity: (self readStreamDo: [ :stream | 
					| result |
					result := [(stream next: 10000) asString]
						on: Error 
						do: [ (stream binary next: 10000) asString ].
					stream size > 10000
						ifTrue: [ result, '  ... truncated ...' ]
						ifFalse: [ result ] ])
]

{ #category : #'*GT-SpotterExtensions-Core' }
FileReference >> spotterPreviewItemsIn: aComposite [
	<spotterPreview: 10>
	
	self isDirectory ifFalse: [ ^ self ].
	aComposite list
		title: [ self gtDisplayString ];
		display: #yourself;
		format: [ :folder | 
			|iconName |			
			iconName := folder isDirectory 
				ifTrue: [ #glamorousFolder ]
				ifFalse: [ #glamorousBrowse ].
			
			GTSpotterItemBrick new 
				text: folder basename; 
				icon: (self iconNamed: iconName); 
				disable; 
				yourself ];
		styled: [ :brick | brick ];
		entity: self directories, self files.
		
	self flag: 'move "entity: self" somewhere else, maybe'
]

{ #category : #'*GT-SpotterExtensions-Core' }
FileReference >> spotterPreviewPngIn: aComposite [
	<spotterPreview: 10>
	
	(self mimeTypes notNil and: [ 
		self mimeTypes first matches: ZnMimeType imagePng ]) 
		ifFalse: [ ^ self ].
	aComposite custom: 
		((self binaryReadStreamDo: [ :stream | PNGReadWriter formFromStream: stream ])
			asAlphaImageMorph asBrick margin: 20)
]

{ #category : #'*GT-SpotterExtensions-Core' }
FileReference >> spotterPreviewStScriptIn: aComposite [
	<spotterPreview: 10>
	
	(self extension = 'st' or: [self extension = 'ph']) 
		ifFalse: [ ^ self ].
	aComposite pharoMethod 
		title: [self gtDisplayString];
		display: #yourself;
		smalltalkClass: [ nil ];
		entity: (self readStreamDo: #contents)
]

{ #category : #'*GT-SpotterExtensions-Core' }
FileReference >> spotterPreviewZipItemsIn: aComposite [
	<spotterPreview: 10>

	| zip |
	self isZipFile ifFalse: [ ^ self ].
	zip:= (FileSystem zip: self) open workingDirectory.
	aComposite list
		title: [ self gtDisplayString ];
		display: #yourself;
		format: [ :folder | 
			|iconName |			
			iconName := folder isDirectory 
				ifTrue: [ #glamorousFolder ]
				ifFalse: [ #glamorousBrowse ].
				
			GTSpotterItemBrick new 
				text: folder basename; 
				icon: (self iconNamed: iconName); 
				disable; 
				yourself ];
		styled: [ :brick | brick ];
		entity: (zip directories, zip files)
]
