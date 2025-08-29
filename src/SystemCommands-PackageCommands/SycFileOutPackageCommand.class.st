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

	packages do: [ :each | each fileOut ]
]
