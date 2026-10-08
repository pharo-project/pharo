Extension { #name : 'Fileouter' }

{ #category : '*SystemCommands-SourceCodeCommands' }
Fileouter class >> fileoutNamed: filename cofigureWith: aBlock [
	self
		deprecated: 'Use #fileoutNamed:configureWith: instead'
		transformWith: '`@rec fileoutNamed: `@arg1 cofigureWith: `@arg2'
			-> '`@rec fileoutNamed: `@arg1 configureWith: `@arg2'.
	^ self fileoutNamed: filename configureWith: aBlock
]

{ #category : '*SystemCommands-SourceCodeCommands' }
Fileouter class >> fileoutNamed: filename configureWith: aBlock [
	| fileRef dialog ok safeName suggestedName |
	safeName := FileNameSanitizer sanitize: filename.
	suggestedName := (safeName asLowercase endsWith: '.st')
		ifTrue: [ safeName ]
		ifFalse: [ safeName , '.st' ].
	StSaveFilePresenter new
		defaultFolder: FileLocator workingDirectory;
		nameText: suggestedName;
		okAction: [ :fileReference |
			| fileouter |
			fileRef := fileReference.
			fileouter := self new.
			aBlock value: fileouter.
			fileouter fileoutOnFileReference: fileReference.
			dialog := SpConfirmDialog new.
			ok := dialog
				title: 'Do you want to browse natively the fileout?';
				message: 'Are you sure?';
				acceptLabel: 'Sure!';
				cancelLabel: 'No, forget it';
				openModal.
			ok ifTrue: [ fileRef openInOSFileBrowser ] ];
		openModal
]
