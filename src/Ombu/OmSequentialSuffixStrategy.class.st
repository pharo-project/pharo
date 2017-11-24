"
Use FileReference>>nextVersion to find the new name.
"
Class {
	#name : #OmSequentialSuffixStrategy,
	#superclass : #OmSessionStoreNamingStrategy,
	#category : #'Ombu-Strategies'
}

{ #category : #accessing }
OmSequentialSuffixStrategy >> nextTo: aName in: aDirectory [
	
	| fileReference |
	fileReference := OmFileStore
		fileReferenceForStoreNamed: aName
		inDirectory: aDirectory.
	
	^ fileReference nextVersion basenameWithoutExtension

]
