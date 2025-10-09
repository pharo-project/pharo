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

	StSaveFilePresenter new
		defaultFolder: FileLocator workingDirectory;
		nameText: 'fileout.st';
		okAction: [ :fileReference |
				| fileouter |
				fileouter := Fileouter new.
				self methods do: [ :each | fileouter addMethod: each ].
				fileouter fileoutOnFileReference: fileReference.
				
				UIManager default
					inform: (String streamContents: [ :s |
									 s
										 nextPutAll: 'Filed out to: ';
										 cr;
										 nextPutAll: fileReference pathString;
										 cr;
										 cr;
										 nextPutAll: 'Click to open location' ])
					actionOnClick: [ fileReference openInOSFileBrowser ] ];
		openModal
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
