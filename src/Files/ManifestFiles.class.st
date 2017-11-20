"
I store metadata for this package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : #ManifestFiles,
	#superclass : #PackageManifest,
	#category : #Files
}

{ #category : #'meta-data - dependency analyser' }
ManifestFiles class >> manuallyResolvedDependencies [
	^ #(#'System-Support' #'Zinc-Character-Encoding-Core' #'FileSystem-Core' #'Multilingual-TextConversion' #'Collections-Abstract' #'System-Platforms')
]
