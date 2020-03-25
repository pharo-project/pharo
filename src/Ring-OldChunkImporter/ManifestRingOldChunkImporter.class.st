"
Manifest for deprecated Ring-Deprecated-ChunkImporter package
"
Class {
	#name : #ManifestRingOldChunkImporter,
	#superclass : #PackageManifest,
	#category : #'Ring-OldChunkImporter-Manifest'
}

{ #category : #'meta-data - dependency analyser' }
ManifestRingOldChunkImporter class >> manuallyResolvedDependencies [
	^ #(#'OpalCompiler-Core' #'FileSystem-Core' #'Collections-Abstract' #'Collections-Strings' #'System-Support')
]
