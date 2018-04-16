Class {
	#name : #ManifestCompression,
	#superclass : #PackageManifest,
	#category : #Compression
}

{ #category : #'meta-data - dependency analyser' }
ManifestCompression class >> manuallyResolvedDependencies [
	^ #(#Jobs #'System-Support' #'System-Localization' #'System-Changes' #'Collections-Abstract' #'Transcript-Core')

]
