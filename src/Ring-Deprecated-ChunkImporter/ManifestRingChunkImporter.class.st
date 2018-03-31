Class {
	#name : #ManifestRingChunkImporter,
	#superclass : #PackageManifest,
	#category : #'Ring-Deprecated-ChunkImporter'
}

{ #category : #deprecation }
ManifestRingChunkImporter class >> isDeprecated [
	^true
]

{ #category : #'meta-data - dependency analyser' }
ManifestRingChunkImporter class >> manuallyResolvedDependencies [
	^ #(#'OpalCompiler-Core' #'FileSystem-Core' #'Collections-Abstract' #'Collections-Strings' #'System-Support')
]
