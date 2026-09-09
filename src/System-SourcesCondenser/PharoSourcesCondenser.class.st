"
I create a more compact version of the .sources file.
I move the source code from all methods to a new .sources file where I keep only one version.
Additionally I will empty the current .changes file.
"
Class {
	#name : 'PharoSourcesCondenser',
	#superclass : 'PharoChangesCondenser',
	#category : 'System-SourcesCondenser',
	#package : 'System-SourcesCondenser'
}

{ #category : 'helper' }
PharoSourcesCondenser class >> condenseNewSources [
	| fileName condenser arch |

	arch := (Smalltalk vm wordSize = 4) ifTrue: [32] ifFalse: [64].

	fileName := 'Pharo{1}.{2}-{3}bit-{4}' format: {SystemVersion current major asString. SystemVersion current minor asString. arch. SystemVersion current commitHashShort asString}.

	condenser := self new.
	condenser newSourceNamed: fileName.
	condenser condense
]

{ #category : 'private - 3 installing' }
PharoSourcesCondenser >> backupOldChanges [
	super backupOldChanges.
	self installFreshChangesFile
]

{ #category : 'accessing' }
PharoSourcesCondenser >> fileIndex [
	"We are working on the .sources file which has index = 1"
	^ 1
]

{ #category : 'private - 3 installing' }
PharoSourcesCondenser >> installFreshChangesFile [

	| changesFile |
	"install a new .changes file"
	changesFile := FileLocator changes.
	changesFile moveTo: (changesFile , 'bak') nextVersion. "Force the creation of a fresh changes file "
	changesFile writeStreamDo: [ :strm | (CodeExporterV100 onStream: strm) writeFileoutVersion ]
]

{ #category : 'public' }
PharoSourcesCondenser >> newSourceNamed: aString [
	"Create a new version of the .sources file by copying the existing file to a new location"
	| originalSourcesFile newSourcesFile |

	"Create a copy with the new name"
	originalSourcesFile := self originalFile resolve asFileReference.
	PharoFilesOpener sourceFileVersionString: aString.
	newSourcesFile := self originalFile.
	
	"If the condensed file is the new file, don't do anything."
	originalSourcesFile = newSourcesFile ifTrue: [ ^ self ].
	
	originalSourcesFile copyTo: newSourcesFile.
	"Use the new sources file"
	SourcesManager current reset
]

{ #category : 'accessing' }
PharoSourcesCondenser >> originalFile [

	^ PharoFilesOpener sourcesFile
]

{ #category : 'private - testing' }
PharoSourcesCondenser >> shouldCondenseMethod: aCompiledMethod [
	"The sources condenser moves ALL method sources to a new file"
	^ aCompiledMethod hasSourcePointer
]

{ #category : 'writing' }
PharoSourcesCondenser >> shouldWriteClassComment: commentRemoteString [

	^ commentRemoteString notNil
]

{ #category : 'accessing' }
PharoSourcesCondenser >> temporaryFile [
	^ 'new.sources' asFileReference nextVersion
]
