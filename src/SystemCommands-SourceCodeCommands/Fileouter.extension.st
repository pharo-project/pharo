Extension { #name : 'Fileouter' }

{ #category : '*SystemCommands-SourceCodeCommands' }
Fileouter class >> fileoutNamed: filename cofigureWith: aBlock [

	| fileRef dialog ok |
	StSaveFilePresenter new
		defaultFolder: FileLocator workingDirectory;
		nameText: filename , '.st';
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
