"
I can write metadataless filetree versions from an ice formed version.
"
Class {
	#name : #IceMetadatalessFileTreeWriter,
	#superclass : #MCFileTreeWriter,
	#category : #'Iceberg-Libgit-Filetree'
}

{ #category : #writing }
IceMetadatalessFileTreeWriter class >> fileOut: aVersion on: aStream [
	(self on: aStream) writeVersion: aVersion
]

{ #category : #accessing }
IceMetadatalessFileTreeWriter >> snapshotWriterClass [
	^ IceMetadatalessStCypressWriter
]

{ #category : #visiting }
IceMetadatalessFileTreeWriter >> writeVersion: aVersion [
	"We have to do that to avoid writing the version info."

	| members |
	directory := (members := MCFileTreeRepository
		parseName: aVersion info name) last.
	self deleteExistingPackageStructureFor: members.
	self fileUtils ensureDirectoryExists: self packageFileDirectory.
	self initializePackageFileDirectoryCache.
	self writeFormat.
	self writePackage: aVersion package.
	self writeDefinitions: aVersion.
	aVersion dependencies do: [ :ea | self writeVersionDependency: ea ]
]
