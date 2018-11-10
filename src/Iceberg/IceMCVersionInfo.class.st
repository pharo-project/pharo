"
Adapter to produce an MCVersionInfo that knows how to (lazily) load ancestors from an Iceberg repository.
"
Class {
	#name : #IceMCVersionInfo,
	#superclass : #MCLazyVersionInfo,
	#instVars : [
		'commit',
		'package'
	],
	#category : #'Iceberg-Adapters'
}

{ #category : #'instance creation' }
IceMCVersionInfo class >> package: package message: commitMessage [
	^ self new fromPackage: package message: commitMessage

]

{ #category : #utilities }
IceMCVersionInfo class >> uuidFromCommit: aCommit package: aPackage. [
	^ self uuidFromSHA: (SHA1 new hashStream: (ReadStream on: aCommit id, aPackage name))

]

{ #category : #utilities }
IceMCVersionInfo class >> uuidFromSHA: cid [
	"Return an adequate uuid from a git commit ID. Copied from GitFileTree."

	^ UUID nilUUID
		in: [ :uuid |
			1 to: 16 do: [ :i | uuid at: i put: (cid at: i) ].
			uuid at: 9 put: (16r80 bitOr: ((uuid at: 9) bitAnd: 16r3F)).	"Type"
			uuid at: 7 put: (16r50 bitOr: ((uuid at: 7) bitAnd: 16r0F)).	"Version 5 : SHA-1 Namespace (git)."
			uuid ]
]

{ #category : #comparing }
IceMCVersionInfo >> = anotherIceMCVersionInfo [

	^ super = anotherIceMCVersionInfo
		and: [ package name = anotherIceMCVersionInfo package name ]
]

{ #category : #accessing }
IceMCVersionInfo >> commit [
	^ commit
]

{ #category : #initialization }
IceMCVersionInfo >> fromCommit: aCommit package: aPackage [
	"This method can be used for first initializing a version info or to update a partially initialized version info."
	"See #fromPackage:message:"
	commit := aCommit.
	package := aPackage.
	
   	date := commit datetime asDate.
   	time := commit datetime asTime.
	
	name := ('{1}-{2}.{3}' format: {
		package name. 
		commit compatibleUsername. 
		commit datetime asUnixTime 
	}).
	
	id := self class uuidFromCommit: aCommit package: aPackage.

   	message := commit comment.
   	author := commit compatibleUsername

]

{ #category : #initialization }
IceMCVersionInfo >> fromPackage: aPackage message: commitMessage [
	"This creates a partial version info. Full version info can not be created until we commit into a repository, 
	but current fileout tools require it, so we first create an incomplete info and after committing it will be completed
	by sending #fromCommit:package:
	
	To improve this implementation we should review the fileout process."

	name := ('{1}-{2}.{3}' format: {aPackage name. Author fullName. 'unknown'}).
	package := aPackage.
	message := commitMessage.
	id := UUID new

]

{ #category : #comparing }
IceMCVersionInfo >> hash [

	^ super hash << 10 + package hash
]

{ #category : #accessing }
IceMCVersionInfo >> iceVersion [
	^ commit versionFor: package
]

{ #category : #initialization }
IceMCVersionInfo >> initialize [
	super initialize.
	date := Date today.
	time := Time now
]

{ #category : #private }
IceMCVersionInfo >> loadAncestorsAndStepChildren [
	self shouldBeImplemented 
]

{ #category : #accessing }
IceMCVersionInfo >> package [
	^ package
]
