"
Some basic classes used in collections: Link, Associtation, Weak*, CharacterSet, SetElement, etc.



!!Character Sets

There are a number of options for holding sets of characters:

Set, CharacterSet and WideCharacterSet can all be used where the set is empty by default and characters are explicitly added.

CharacterSetComplement is used where all characters are in the set by default and characters are explicitly excluded.

"
Class {
	#name : #ManifestCollectionsSupport,
	#superclass : #PackageManifest,
	#category : #'Collections-Support-Manifest'
}

{ #category : #'meta-data' }
ManifestCollectionsSupport class >> dependencies [
	^ #(#'Collections-Strings' #'Collections-Native' #'Collections-Unordered' #Kernel #'Collections-Abstract')
]

{ #category : #'meta-data - dependency analyser' }
ManifestCollectionsSupport class >> manuallyResolvedDependencies [
	^ #(#'Collections-Streams')
]

{ #category : #'meta-data' }
ManifestCollectionsSupport class >> packageName [
	^ #'Collections-Support'
]
