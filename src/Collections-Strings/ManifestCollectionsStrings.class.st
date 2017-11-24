"
I store metadata for this package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : #ManifestCollectionsStrings,
	#superclass : #PackageManifest,
	#category : #'Collections-Strings'
}

{ #category : #'meta-data' }
ManifestCollectionsStrings class >> dependencies [
	^ #(#'Collections-Streams' #'Collections-Weak' #'Collections-Native' #'Collections-Support' #Kernel #'Collections-Sequenceable' #'Multilingual-Encodings' #'Collections-Abstract')
]

{ #category : #'meta-data' }
ManifestCollectionsStrings class >> description [
	^ 'Contains String, ByteString, Symbol, ByteSymbol, WideSymbol and WideString'
]

{ #category : #'meta-data - dependency analyser' }
ManifestCollectionsStrings class >> manuallyResolvedDependencies [
	^ #(#'System-Support' #'AST-Core' #'Random-Core' #'OpalCompiler-Core')
]

{ #category : #'meta-data' }
ManifestCollectionsStrings class >> packageName [
	^ #'Collections-Strings'
]
