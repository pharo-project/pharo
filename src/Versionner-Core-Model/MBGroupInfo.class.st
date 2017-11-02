"
A MBGroupInfo holds information on a group defintiion.

Instance Variables
	groupContents:		an array with group contents as literals .

"
Class {
	#name : #MBGroupInfo,
	#superclass : #MBSpecInfo,
	#instVars : [
		'groupContents'
	],
	#category : 'Versionner-Core-Model'
}

{ #category : #accessing }
MBGroupInfo class >> helpLabel [

	^'Group'
]

{ #category : #accessing }
MBGroupInfo >> groupContentsIfLoaded [

	| version loadedPackageNames groupPackageNames |
	groupContents ~~ nil ifTrue: [ ^groupContents ].
	groupContents := #().
	self configurationInfo ifNil: [ ^ groupContents ].
	(version := self configurationInfo currentVersion) ifNil: [ ^groupContents ].
	loadedPackageNames := version defaultPackageNamesToLoad: #().
	loadedPackageNames remove: 'default' ifAbsent: [].
	groupPackageNames := (version packagesForSpecNamed: self name) collect: [:spc | spc name ].
	groupPackageNames do: [:nm | (loadedPackageNames includes: nm) ifFalse: [ ^groupContents ] ].
	^groupContents := self spec includes
]

{ #category : #accessing }
MBGroupInfo >> name [
	^ self spec 
		ifNil: [ 'nil' ]
		ifNotNil: [ :specif | specif name ]
]

{ #category : #accessing }
MBGroupInfo >> name: aName [

	self shouldNotImplement
]

{ #category : #printing }
MBGroupInfo >> printOn: aStream [
	| loadedPackageNames |
	super printOn: aStream.
	(loadedPackageNames := self groupContentsIfLoaded) isEmpty
		ifTrue: [ aStream nextPutAll: ' ---' ]
		ifFalse: [ 
			aStream nextPutAll: ' [ '.
			loadedPackageNames do: [:pkgName | aStream nextPutAll: pkgName, ' '  ].
			aStream nextPut: $] ].
]

{ #category : #accessing }
MBGroupInfo >> typeLabel [

	^'group'
]
