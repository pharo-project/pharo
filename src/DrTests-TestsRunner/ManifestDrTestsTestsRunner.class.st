"
Please describe the package using the class comment of the included manifest class. The manifest class also includes other additional metadata for the package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : 'ManifestDrTestsTestsRunner',
	#superclass : 'PackageManifest',
	#category : 'DrTests-TestsRunner-Manifest',
	#package : 'DrTests-TestsRunner',
	#tag : 'Manifest'
}

{ #category : 'meta-data - dependency analyser' }
ManifestDrTestsTestsRunner class >> manuallyResolvedDependencies [

	<ignoreForCoverage>
	^ #(#'System-Localization' #'SUnit-UI')
]
