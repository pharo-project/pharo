"
Please describe the package using the class comment of the included manifest class. The manifest class also includes other additional metadata for the package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : 'ManifestKernelCodeModel',
	#superclass : 'PackageManifest',
	#category : 'Kernel-CodeModel-Manifest',
	#package : 'Kernel-CodeModel',
	#tag : 'Manifest'
}

{ #category : 'meta-data - dependency analyser' }
ManifestKernelCodeModel class >> manuallyResolvedDependencies [

	<ignoreForCoverage>
	^ #(#'Shift-ClassBuilder')
]
