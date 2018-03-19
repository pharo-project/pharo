Class {
	#name : #ManifestRingChunkImporter,
	#superclass : #PackageManifest,
	#category : #RingChunkImporter
}

{ #category : #'meta-data - dependency analyser' }
ManifestRingChunkImporter class >> manuallyResolvedDependencies [
	^ #(#'OpalCompiler-Core' #'FileSystem-Core' #'Collections-Abstract' #'Collections-Strings' #'System-Support')
]
