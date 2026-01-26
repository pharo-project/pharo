"
Please describe the package using the class comment of the included manifest class. The manifest class also includes other additional metadata for the package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : 'ManifestFFIKernel',
	#superclass : 'PackageManifest',
	#category : 'FFI-Kernel-Manifest',
	#package : 'FFI-Kernel',
	#tag : 'Manifest'
}

{ #category : 'meta-data - dependency analyser' }
ManifestFFIKernel class >> manuallyResolvedDependencies [

	<ignoreForCoverage>
	^ #(#'Zinc-Character-Encoding-Core' #'Transcript-NonInteractive' #'Collections-Abstract' #'System-Platforms' #'System-Support')
]
