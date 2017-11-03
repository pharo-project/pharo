"
I store metadata for this package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : #ManifestRingChunkImporter,
	#superclass : #PackageManifest,
	#category : #RingChunkImporter
}

{ #category : #'meta-data - dependency analyser' }
ManifestRingChunkImporter class >> manuallyResolvedDependencies [
	^ #(#'OpalCompiler-Core' #'FileSystem-Core' #'Collections-Abstract' #'Collections-Strings' #'System-Support')
]
