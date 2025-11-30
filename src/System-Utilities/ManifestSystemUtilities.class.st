"
Please describe the package using the class comment of the included manifest class. The manifest class also includes other additional metadata for the package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : 'ManifestSystemUtilities',
	#superclass : 'PackageManifest',
	#category : 'System-Utilities-Manifest',
	#package : 'System-Utilities',
	#tag : 'Manifest'
}

{ #category : 'meta-data - dependency analyser' }
ManifestSystemUtilities class >> manuallyResolvedDependencies [

	<ignoreForCoverage>
	^ #(#'Kernel-CodeModel' #'Text-Core' #'Collections-Abstract')
]
