"
My role is to open the .sources and .changes files. My only public methods are in the 'public' protocol. The most important method is #checkAndOpenSourcesAndChanges.
"
Class {
	#name : #PharoFilesOpener,
	#superclass : #Object,
	#instVars : [
		'shouldInformAboutReadOnlyChanges'
	],
	#classVars : [
		'Default'
	],
	#category : #'System-Sources'
}

{ #category : #singleton }
PharoFilesOpener class >> default [
	^ Default ifNil: [ Default := self new ]
]

{ #category : #singleton }
PharoFilesOpener class >> reset [
	Default := nil
]

{ #category : #messages }
PharoFilesOpener >> badContentMsg [
	^ '&fileRef
has been injured by an unpacking utility.  Crs were changed to CrLfs.
Please set the preferences in your decompressing program to 
"do not convert text files" and unpack the system again.'
]

{ #category : #messages }
PharoFilesOpener >> cannotLocateMsg [
	^ 'Pharo cannot locate &fileRef.

Please check that the file is named properly and is in the
same directory as this image.'
]

{ #category : #messages }
PharoFilesOpener >> cannotWriteMsg [
	^ 'Pharo cannot write to &fileRef.

Please check that you have write permission for this file.

You won''t be able to save this image correctly until you fix this.'
]

{ #category : #public }
PharoFilesOpener >> changesFileOrNil [

	^ self changesFileOrNilReadOnly: false
]

{ #category : #public }
PharoFilesOpener >> changesFileOrNilReadOnly: readOnly [

	^ self changesFileOrNilReadOnly: readOnly silent: false
]

{ #category : #public }
PharoFilesOpener >> changesFileOrNilReadOnly: readOnly silent: silent [
	| changesFile |
	changesFile := self openChanges: self changesName readOnly: readOnly.
	(changesFile isNil and: [ silent not ])
		ifTrue: [ self informProblemInChanges: self cannotLocateMsg ].

	silent
		ifTrue: [ ^ changesFile ].

	(changesFile isReadOnly and: [ self shouldInformAboutReadOnlyChanges ])
		ifTrue: [ self informProblemInChanges: self cannotWriteMsg ].
	((changesFile next: 200) includesSubstring: String crlf)
		ifTrue: [ self informProblemInChanges: self badContentMsg ].
	^ changesFile
]

{ #category : #delegated }
PharoFilesOpener >> changesName [
	^ Smalltalk changesName
]

{ #category : #helper }
PharoFilesOpener >> ignoreIfFail: aBlock [
	^ [ aBlock value ] ifError: [  ]
]

{ #category : #'user interaction' }
PharoFilesOpener >> inform: msg withChangesRef: fileRef [
	self inform: msg withRef: 'the changes file named ' , fileRef
]

{ #category : #'user interaction' }
PharoFilesOpener >> inform: msg withRef: fileRef [
	self inform: (msg copyReplaceAll: '&fileRef' with: fileRef)
]

{ #category : #'user interaction' }
PharoFilesOpener >> inform: msg withSourceRef: fileRef [
	self inform: msg withRef: 'the sources file named ' , fileRef
]

{ #category : #'open sources' }
PharoFilesOpener >> informCannotLocateSources [
	| msg |
	msg := self cannotLocateMsg.
	Smalltalk os isMacOS
		ifTrue: [ 
			msg := msg
				,
					'
Make sure the sources file is not an Alias.' ].
	self inform: msg withSourceRef: self sourcesName
]

{ #category : #'user interaction' }
PharoFilesOpener >> informProblemInChanges: msg [
	self inform: msg withChangesRef: self changesName
]

{ #category : #delegated }
PharoFilesOpener >> lastImagePath [
	^ Smalltalk lastImagePath
]

{ #category : #'open changes' }
PharoFilesOpener >> openChanges: changesPath readOnly: readOnly [

	^ (SourceFile on: changesPath)
		tryOpenReadOnly: readOnly;
		yourself
]

{ #category : #'open sources' }
PharoFilesOpener >> openSources [
	^ self openSources: self sourcesName forImage: self lastImagePath
]

{ #category : #'open sources' }
PharoFilesOpener >> openSources: fullSourcesName forImage: imagePath [
	"Look in various places for a sources file, return an open stream to it."

	| sourcesFile sourcesName |
	sourcesFile := fullSourcesName asFileReference.
	sourcesName := sourcesFile basename.
	
	"look for the sources file or an alias to it in the VM's directory"
	OSPlatform current potentialLocationsOfSourcesFile do: [ :dir |
		self ignoreIfFail: [ 
			(sourcesFile := dir / sourcesName) exists
				ifTrue: [ sourcesFile readStream ifNotNil: [ :stream | ^ stream ] ] ] ].
			
	"look for the sources file or an alias to it in the image directory"
	(sourcesFile := imagePath asFileReference parent / sourcesName) exists
		ifTrue: [ ^ sourcesFile readStream ifNotNil: [ :stream | ^ stream ] ].
		
	"look for the sources in the current directory"
	(sourcesFile := sourcesName asFileReference) exists
		ifTrue: [ ^ sourcesFile readStream ifNotNil: [ :stream | ^ stream ] ].
		
	^ nil
]

{ #category : #public }
PharoFilesOpener >> setInformAboutReadOnlyChanges [
	"Make sure the user is informed when the .changes file can not be written to."
	shouldInformAboutReadOnlyChanges := true
]

{ #category : #public }
PharoFilesOpener >> shouldInformAboutReadOnlyChanges [
	"Answer true if and only if the user must be informed when the .changes file can not be written to."
	^ shouldInformAboutReadOnlyChanges ifNil: [ shouldInformAboutReadOnlyChanges := true ]
]

{ #category : #public }
PharoFilesOpener >> sourcesFileOrNil [
	| sources |
	sources := self openSources.
	^ (sources isNil or: [ sources atEnd ])
		ifTrue: [ 
			self informCannotLocateSources.
			nil ]
		ifFalse: [ sources ]
]

{ #category : #delegated }
PharoFilesOpener >> sourcesName [
	^ Smalltalk sourcesName
]

{ #category : #public }
PharoFilesOpener >> unsetInformAboutReadOnlyChanges [
	"Make sure the user is *not* informed when the .changes file can not be written to."
	shouldInformAboutReadOnlyChanges := false
]
