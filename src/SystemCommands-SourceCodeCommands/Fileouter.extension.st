Extension { #name : 'Fileouter' }

{ #category : '*SystemCommands-SourceCodeCommands' }
Fileouter class >> fileoutNamed: filename cofigureWith: aBlock [

	StSaveFilePresenter new
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
						nextPutAll: fileReference pathString;
						cr;
						cr;
						nextPutAll: 'Click to open location' ].

				UIManager default
					inform: message
					actionOnClick: [ fileReference openInOSFileBrowser ] ];
		openModal
]