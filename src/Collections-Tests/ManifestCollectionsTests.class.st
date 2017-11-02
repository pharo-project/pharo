"
I store metadata for this package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : #ManifestCollectionsTests,
	#superclass : #PackageManifest,
	#category : 'Collections-Tests'
}

{ #category : #meta-data }
ManifestCollectionsTests class >> dependencies [
	^ #(#'Collections-Streams' #'System-Finalization' #'Collections-Native' #'Collections-Abstract' #'Collections-Weak' #'Collections-Stack' #'Collections-Strings' #Compiler #'Collections-Sequenceable' #'SUnit-Core' #Kernel #'Graphics-Primitives' #'Collections-Support' #'Collections-Unordered' #'System-Support' #'Collections-Atomic' #'Multilingual-Encodings')
]

{ #category : #meta-data }
ManifestCollectionsTests class >> description [
	^ 'I contain tests for collections'
]

{ #category : #'meta-data - dependency analyser' }
ManifestCollectionsTests class >> manuallyResolvedDependencies [
	^ #(#'Collections-Arithmetic' #'Transcript-Core' #'OpalCompiler-Core')
]

{ #category : #meta-data }
ManifestCollectionsTests class >> packageName [
	^ #CollectionsTests
]
