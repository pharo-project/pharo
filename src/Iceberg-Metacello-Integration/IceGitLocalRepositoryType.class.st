"
I resolve git local repositories (repositories already existing on your disk).
A gitlocal repositoy is composed of: 

gitlocal://full/path/to/repository

gitlocal  		- The git local identifier
full/path/...	- This is a path to the place where the sources are. 
	
For example, if you have a  repository in '/dev/voyage', which also has sources in subdirectory 'mc', your full path will be: '/dev/voyage/mc'. A part of my responsibilities is to find the git root  in the path provided.

Example: 
------------
A script to install voyage using this would like more or less like this:

Metacello new
	repository: 'gitlocal://Users/esteban/Dev/Repository/voyage/mc';
	baseline: 'Voyage';
	load: 'mongo tests'.

"
Class {
	#name : #IceGitLocalRepositoryType,
	#superclass : #IceMetacelloRepositoryType,
	#category : #'Iceberg-Metacello-Integration'
}

{ #category : #accessing }
IceGitLocalRepositoryType class >> type [
	^ 'gitlocal'
]

{ #category : #testing }
IceGitLocalRepositoryType >> isGitRoot: aReference [
	^ IceRepositoryCreator isGitRoot: aReference
]

{ #category : #accessing }
IceGitLocalRepositoryType >> mcRepository [
	| repository tuple extractedLocation |
	
	tuple := self splitRootAndSubdirectoryFromLocation.
	extractedLocation := tuple first asFileReference asAbsolute.
	repository := IceRepository registry 
		detect: [ :each | 
			each location notNil 
				and: [ each location asAbsolute = extractedLocation ] ]
		ifNone: [ 
			(IceRepositoryCreator new 
				location: tuple first;
				subdirectory: tuple second;
				ensureProjectFile;
				createRepository)
				register ].
			
	^ repository metacelloAdapter
]

{ #category : #private }
IceGitLocalRepositoryType >> splitRootAndSubdirectoryFromLocation [
	| root subDir |
	(self location beginsWith: (self class type, '://'))
		ifFalse: [ self error: 'Invalid URL (It should be ', self class type, '://...)' ].
	root := (self location allButFirst: self class type size + 3) asFileReference.
	subDir := #().
	[ root isRoot or: [ self isGitRoot: root ] ] 
	whileFalse: [ 
		(root asAbsolute = FileSystem workingDirectory)
			ifFalse: [ subDir := subDir copyWithFirst: root basename ].
		root := root parent ].
	root isRoot ifTrue: [ self error: 'I can''t find a .git/config file.' ].
	
	^ { 
		root. "a FileReference"
		String streamContents: [ :stream | subDir asStringOn: stream delimiter: '/' ] "a String" }
]
