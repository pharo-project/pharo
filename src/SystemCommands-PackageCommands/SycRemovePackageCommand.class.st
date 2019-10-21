"
I am a command to remove given packages
"
Class {
	#name : #SycRemovePackageCommand,
	#superclass : #SycPackageCommand,
	#category : #'SystemCommands-PackageCommands'
}

{ #category : #accessing }
SycRemovePackageCommand class >> defaultMenuIconName [ 
	^ #delete
]

{ #category : #accessing }
SycRemovePackageCommand >> defaultMenuIconName [
	^#removeIcon
]

{ #category : #accessing }
SycRemovePackageCommand >> defaultMenuItemName [
	^'Remove'
]

{ #category : #execution }
SycRemovePackageCommand >> execute [
			
	packages do: [ :each | each removeFromSystem ]
]

{ #category : #execution }
SycRemovePackageCommand >> prepareFullExecutionInContext: aToolContext [
	| packagesWithClasses names answer |
	super prepareFullExecutionInContext: aToolContext.
	
	packagesWithClasses := packages reject: [ :each | each isEmpty].
	packagesWithClasses ifNotEmpty: [ 	
		names := (packagesWithClasses collect: [:package | package name ]) joinUsing: ', '.
		answer := UIManager default confirm: (
			'Are you sure you want to delete the selection(s)?', String cr, names, ' still contains classes.').
		answer ifFalse: [ CmdCommandAborted signal ]]
]
