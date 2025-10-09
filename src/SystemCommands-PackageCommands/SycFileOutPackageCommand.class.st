"
Requests to save a textual version of the selected package to a new file in chunk format and .st extension. 
"
Class {
	#name : 'SycFileOutPackageCommand',
	#superclass : 'SycPackageCommand',
	#category : 'SystemCommands-PackageCommands',
	#package : 'SystemCommands-PackageCommands'
}

{ #category : 'activation' }
SycFileOutPackageCommand class >> fullBrowserMenuActivation [

	<classAnnotation>
	^ CmdContextMenuActivation
		  byItemOf: CmdExtraMenuGroup
		  for: Package asCalypsoItemContext 
]

{ #category : 'accessing' }
SycFileOutPackageCommand >> defaultMenuIconName [

	^ #save
]

{ #category : 'accessing' }
SycFileOutPackageCommand >> defaultMenuItemName [

	^ 'File out'
]

{ #category : 'execution' }
SycFileOutPackageCommand >> execute [

	StSaveFilePresenter new
		defaultFolder: FileLocator workingDirectory;
		nameText: 'fileout.st';
		okAction: [ :fileReference |
				| fileouter |
				fileouter := Fileouter new.
				packages do: [ :each | fileouter addFullPackage: each ].
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
