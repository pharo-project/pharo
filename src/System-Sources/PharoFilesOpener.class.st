"
My role is to open the .sources and .changes files. My only public methods are in the 'public' protocol. The most important method is #checkAndOpenSourcesAndChanges.
"
Class {
	#name : 'PharoFilesOpener',
	#superclass : 'Object',
	#classVars : [
		'Default',
		'SourceFileVersionString'
	],
	#category : 'System-Sources-Utilities',
	#package : 'System-Sources',
	#tag : 'Utilities'
}

{ #category : 'singleton' }
PharoFilesOpener class >> default [
	^ Default ifNil: [ Default := self new ]
]

{ #category : 'class initialization' }
PharoFilesOpener class >> reset [
	Default := nil
]

{ #category : 'image, changes name' }
PharoFilesOpener class >> sourceFileVersionString [

	^ SourceFileVersionString
]

{ #category : 'image, changes name' }
PharoFilesOpener class >> sourceFileVersionString: aString [

	SourceFileVersionString := aString
]

{ #category : 'accessing' }
PharoFilesOpener class >> sourcesFile [
	"Answer the full path to the version-stable source code"

	^ (FileLocator imageDirectory / (self sourceFileVersionString , '.' , self sourcesSuffix)) asFileReference
]

{ #category : 'accessing' }
PharoFilesOpener class >> sourcesName [
	"Answer the full path to the version-stable source code"

	^ self sourcesFile fullName
]

{ #category : 'accessing' }
PharoFilesOpener class >> sourcesSuffix [

	^ 'sources'
]

{ #category : 'messages' }
PharoFilesOpener >> badContentMsg [
	^ '&fileRef
has been injured by an unpacking utility.  Crs were changed to CrLfs.
Please set the preferences in your decompressing program to
"do not convert text files" and unpack the system again.'
]

{ #category : 'messages' }
PharoFilesOpener >> cannotLocateMsg [
	^ 'Pharo cannot locate &fileRef.

Please check that the file is named properly and is in the
same directory as this image.'
]

{ #category : 'messages' }
PharoFilesOpener >> cannotWriteMsg [
	^ 'Pharo cannot write to &fileRef.

Please check that you have write permission for this file.

You won''t be able to save this image correctly until you fix this.'
]

{ #category : 'public' }
PharoFilesOpener >> changesFileOrNil [

	^ self changesFileOrNilReadOnly: false
]

{ #category : 'public' }
PharoFilesOpener >> changesFileOrNilReadOnly: readOnly [

	| changesFile |
	changesFile := (self locateSources: self changesName forImage: self lastImagePath)
		               tryOpenReadOnly: readOnly;
		               yourself.

	changesFile ifNil: [ self informProblemInChanges: self cannotLocateMsg ].

	changesFile isOpen ifFalse: [ ^ nil ].

	changesFile isReadOnly ifTrue: [ self informProblemInChanges: self cannotWriteMsg ].
	((changesFile next: 200) includesSubstring: String crlf) ifTrue: [ self informProblemInChanges: self badContentMsg ].
	^ changesFile
]

{ #category : 'delegated' }
PharoFilesOpener >> changesName [

	^ FileLocator changes basename
]

{ #category : 'user interaction' }
PharoFilesOpener >> inform: msg withRef: fileRef [

	InformativeNotification signal: (msg copyReplaceAll: '&fileRef' with: fileRef)
]

{ #category : 'open sources' }
PharoFilesOpener >> informCannotLocateSources [

	| msg |
	msg := self cannotLocateMsg.
	Smalltalk os isMacOS ifTrue: [
			msg := msg , '
Make sure the sources file is not an Alias.' ].
	self inform: msg withRef: 'the sources file named ' , self sourcesName
]

{ #category : 'user interaction' }
PharoFilesOpener >> informProblemInChanges: msg [

	self inform: msg withRef: 'the changes file named ' , self changesName
]

{ #category : 'delegated' }
PharoFilesOpener >> lastImagePath [
	^ Smalltalk lastImagePath
]

{ #category : 'open sources' }
PharoFilesOpener >> locateChanges [
	^ self locateSources: self changesName forImage: self lastImagePath
]

{ #category : 'open sources' }
PharoFilesOpener >> locateSources [
	^ self locateSources: self sourcesName forImage: self lastImagePath
]

{ #category : 'open sources' }
PharoFilesOpener >> locateSources: fullSourcesName forImage: imagePath [
	"Look in various places for a sources file, return an open stream to it."

	| locations |
	locations := {
		FileLocator imageDirectory.
		FileLocator workingDirectory
	}.

	^ (SourceFile lookupFileNamed: fullSourcesName potentialLocations: locations)
]

{ #category : 'open sources' }
PharoFilesOpener >> openSources [
	"Look in various places for a sources file, return an open stream to it."

	^ (self locateSources: self sourcesName forImage: self lastImagePath)
		  tryOpenReadOnly: true;
		  yourself
]

{ #category : 'public' }
PharoFilesOpener >> sourcesFileOrNil [
	| sources |
	sources := self openSources.
	^ (sources isNil or: [ sources closed ])
		ifTrue: [
			self informCannotLocateSources.
			nil ]
		ifFalse: [ sources ]
]

{ #category : 'accessing' }
PharoFilesOpener >> sourcesName [
	^ self class sourcesName
]
