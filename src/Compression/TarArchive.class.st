"
This is a kind of archive that uses the TAR format (popular in Unix). It is here as a placeholder.
"
Class {
	#name : #TarArchive,
	#superclass : #Archive,
	#category : #Compression-Archives
}

{ #category : #private }
TarArchive >> memberClass [
	^TarArchiveMember
]
