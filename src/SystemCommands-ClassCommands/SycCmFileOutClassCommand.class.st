"
I am a command to file out given objects.
I simply call #fileOut for each of them.
 
Internal Representation and Key Implementation Points.

## Instance Variables

- codeObjects:		<Colletion of<Object>>
"
Class {
	#name : 'SycCmFileOutClassCommand',
	#superclass : 'SycClassExtraCmCommand',
	#instVars : [
		'codeObjects'
	],
	#category : 'SystemCommands-ClassCommands-Extra',
	#package : 'SystemCommands-ClassCommands',
	#tag : 'Extra'
}

{ #category : 'accessing' }
SycCmFileOutClassCommand >> description [

	^ 'Requests to save a textual version of the selected class to a new file in chunk format and .st extension. Instance and class side methods are both saved'
]

{ #category : 'executing' }
SycCmFileOutClassCommand >> executeRefactoring [

	StSaveFilePresenter new
		defaultFolder: FileLocator workingDirectory;
		nameText: 'fileout.st';
		okAction: [ :fileReference |
				| fileouter |
				fileouter := Fileouter new.
				codeObjects do: [ :each | fileouter addFullClass: each ].
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
SycCmFileOutClassCommand >> name [

	^ 'File out'
]

{ #category : 'preparation' }
SycCmFileOutClassCommand >> prepareFullExecution [

	super prepareFullExecution.
	codeObjects := context selectedObjects
]
