"
Manifest for deprecated Ring-Deprecated-ChunkImporter package
"
Class {
	#name : #ManifestRingChunkImporter,
	#superclass : #PackageManifest,
	#category : #'Ring-OldChunkImporter-Manifest'
}

{ #category : #deprecation }
ManifestRingChunkImporter class >> isDeprecated [
	^true
]

{ #category : #'meta-data - dependency analyser' }
ManifestRingChunkImporter class >> manuallyResolvedDependencies [
	^ #(#'OpalCompiler-Core' #'FileSystem-Core' #'Collections-Abstract' #'Collections-Strings' #'System-Support')
]
