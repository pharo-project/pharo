"
I store metadata for this package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : #ManifestCollectionsAbstract,
	#superclass : #PackageManifest,
	#category : #Collections-Abstract
}

{ #category : #meta-data }
ManifestCollectionsAbstract class >> dependencies [
	^ #(#'Regex-Core' #'Collections-Strings' #'Collections-Streams' #'Collections-Support' #'Collections-Unordered' #Kernel #'Collections-Sequenceable' #'Collections-Native')
]

{ #category : #'meta-data - dependency analyser' }
ManifestCollectionsAbstract class >> manuallyResolvedDependencies [
	^ #(#'Collections-Arithmetic')
]

{ #category : #meta-data }
ManifestCollectionsAbstract class >> packageName [
	^ #'Collections-Abstract'
]
