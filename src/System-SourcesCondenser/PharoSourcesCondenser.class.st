"
I create a more compact version of the .sources file.
I move the source code from all methods to a new .sources file where I keep only one version.
Additionally I will empty the current .changes file.
"
Class {
	#name : #PharoSourcesCondenser,
	#superclass : #PharoChangesCondenser,
	#category : #'System-SourcesCondenser'
}

{ #category : #helper }
PharoSourcesCondenser class >> condenseNewSources [
	| fileName condenser arch |

	arch := (Smalltalk vm wordSize = 4) ifTrue: [32] ifFalse: [64].
	
	fileName := 'Pharo{1}.{2}-{3}bit-{4}' format: {SystemVersion current major asString. SystemVersion current minor asString. arch. SystemVersion current commitHashShort asString}.
	
	condenser := self new.
	condenser newSourceNamed: fileName.
	condenser condense.
]

{ #category : #'private - 3 installing' }
PharoSourcesCondenser >> backupOldChanges [
	super backupOldChanges.
	self installFreshChangesFile.
]

{ #category : #accessing }
PharoSourcesCondenser >> fileIndex [
	"We are working on the .sources file which has index = 1"
	^ 1
]

{ #category : #'private - 3 installing' }
PharoSourcesCondenser >> installFreshChangesFile [
	| changesFile |
	
	"install a new .changes file"
	changesFile := Smalltalk changesFile.
	changesFile moveTo: (changesFile , 'bak') nextVersion.
	
	"Force the creation of a fresh changes file "
	changesFile writeStreamDo: [ :strm |
		 "Nothing, just create the file" ].
]

{ #category : #public }
PharoSourcesCondenser >> newSourceNamed: aString [
	"Create a new version of the .sources file by copying the existing file to a new location"
	| originalSourcesFile newSourcesFile |
	
	"Create a copy with the new name"
	originalSourcesFile := self originalFile resolve asFileReference.
	Smalltalk sourceFileVersionString: aString.
	newSourcesFile := Smalltalk sourcesFile asFileReference.

	originalSourcesFile copyTo: newSourcesFile.
	"Use the new sources file"
	Smalltalk openSourceFiles.
]

{ #category : #accessing }
PharoSourcesCondenser >> originalFile [
	^ Smalltalk sourcesFile asFileReference
]

{ #category : #'private - testing' }
PharoSourcesCondenser >> shouldCondenseMethod: aCompiledMethod [
	"The sources condenser moves ALL method sources to a new file"
	^ aCompiledMethod hasSourcePointer
]

{ #category : #accessing }
PharoSourcesCondenser >> temporaryFile [
	^ 'new.sources' asFileReference nextVersion
]

{ #category : #'private - 3 installing' }
PharoSourcesCondenser >> updateQuitPosition [
	
	"We use an empty .changes file, hence the last quit position is 0"
	
	Smalltalk lastQuitLogPosition: 0.
]

{ #category : #writing }
PharoSourcesCondenser >> writeClassComment: aClass [

	| organizer commentRemoteString stamp |

	organizer := aClass organization.
	commentRemoteString := organizer commentRemoteString.

	commentRemoteString ifNil: [ ^ self ].

	self
		nextCommentChunkDo: [ :strm | 
			strm
				nextPutAll: aClass name;
				nextPutAll: ' commentStamp: '.
			stamp := organizer commentStamp ifNil: [ '<historical>' ].
			stamp storeOn: strm
			].

	self writeRemoteString: organizer classComment for: aClass
]
