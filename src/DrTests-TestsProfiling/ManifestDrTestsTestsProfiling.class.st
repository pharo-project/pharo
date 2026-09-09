"
Please describe the package using the class comment of the included manifest class. The manifest class also includes other additional metadata for the package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : 'ManifestDrTestsTestsProfiling',
	#superclass : 'PackageManifest',
	#category : 'DrTests-TestsProfiling-Manifest',
	#package : 'DrTests-TestsProfiling',
	#tag : 'Manifest'
}

{ #category : 'meta-data - dependency analyser' }
ManifestDrTestsTestsProfiling class >> manuallyResolvedDependencies [

	<ignoreForCoverage>
	^ #(#'System-Localization')
]
