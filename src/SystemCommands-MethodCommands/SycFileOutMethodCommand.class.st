"
Requests to save a textual version of the selected class to a new file in chunk format and .st extension. Instance and class side methods are both saved
"
Class {
	#name : 'SycFileOutMethodCommand',
	#superclass : 'SycMethodExtraCmCommand',
	#category : 'SystemCommands-MethodCommands',
	#package : 'SystemCommands-MethodCommands'
}

{ #category : 'executing' }
SycFileOutMethodCommand >> applyCommandResult [

	| fileRef dialog ok |
	StSaveFilePresenter new
		defaultFolder: FileLocator workingDirectory;
		nameText: 'methodsfileout.st';
		okAction: [ :fileReference |
				| fileouter |
				fileRef := fileReference.
				fileouter := Fileouter new.
				self methods do: [ :each | fileouter addMethod: each ].
				fileouter fileoutOnFileReference: fileReference ];
		openModal.
	dialog := SpConfirmDialog new.
	ok := dialog
		      title: 'Do you want to browse natively the fileout?';
		      message: 'Are you sure?';
		      acceptLabel: 'Sure!';
		      cancelLabel: 'No, forget it';
		      openModal.
	ok ifTrue: [ fileRef openInOSFileBrowser ]
]

{ #category : 'accessing' }
SycFileOutMethodCommand >> description [

	^ 'Requests to save a textual version of the selected class to a new file in chunk format and .st extension. Instance and class side methods are both saved'
]

{ #category : 'accessing' }
SycFileOutMethodCommand >> icon [

	^ self iconNamed: #save
]

{ #category : 'accessing' }
SycFileOutMethodCommand >> name [

	^ 'File out'
]
