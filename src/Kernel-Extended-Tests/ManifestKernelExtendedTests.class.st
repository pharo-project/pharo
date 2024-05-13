"
I store metadata for this package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : 'ManifestKernelExtendedTests',
	#superclass : 'PackageManifest',
	#category : 'Kernel-Extended-Tests-Manifest',
	#package : 'Kernel-Extended-Tests',
	#tag : 'Manifest'
}

{ #category : 'meta-data - dependency analyser' }
ManifestKernelExtendedTests class >> manuallyResolvedDependencies [

	<ignoreForCoverage>
	^ #(#'Math-Operations-Extensions')
]
