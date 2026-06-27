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

	| filename |
	filename := packages size = 1
		            ifTrue: [ packages first name ]
		            ifFalse: [ 'packagefileout' ].

	^ Fileouter
		fileoutNamed: filename
		cofigureWith: [ :fileouter |
			packages do: [ :each | fileouter addFullPackage: each ] ]
]
