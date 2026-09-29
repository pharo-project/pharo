Extension { #name : 'Fileouter' }

{ #category : '*SystemCommands-SourceCodeCommands' }
Fileouter class >> fileoutNamed: filename cofigureWith: aBlock [

	| presenter |
	presenter := StSaveFilePresenter new.
	presenter
		defaultFolder: FileLocator workingDirectory;
		nameText: filename , '.st';
		okAction: [ :fileReference |
			| fileouter message |
			fileouter := self new.
			aBlock value: fileouter.
			fileouter fileoutOnFileReference: fileReference.

			message := String streamContents: [ :s |
				s
					nextPutAll: 'Filed out to: ';
					cr;
					nextPutAll: fileReference pathString ].

			presenter application
				inform: message
				action: 'Open location'
				onClick: [ fileReference openInOSFileBrowser ] ];
		openModal
]
