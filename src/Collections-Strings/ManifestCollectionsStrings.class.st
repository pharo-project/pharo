"
Contains String, ByteString, Symbol, ByteSymbol, WideSymbol and WideString
"
Class {
	#name : #ManifestCollectionsStrings,
	#superclass : #PackageManifest,
	#category : #'Collections-Strings-Manifest'
}

{ #category : #'meta-data' }
ManifestCollectionsStrings class >> dependencies [
	^ #(#'Collections-Streams' #'Collections-Weak' #'Collections-Native' #'Collections-Support' #Kernel #'Collections-Sequenceable' #'Multilingual-Encodings' #'Collections-Abstract')
]

{ #category : #'meta-data - dependency analyser' }
ManifestCollectionsStrings class >> manuallyResolvedDependencies [
	^ #(#'System-Support' )
]

{ #category : #'meta-data' }
ManifestCollectionsStrings class >> packageName [
	^ #'Collections-Strings'
]
