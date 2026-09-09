"
Please describe the package using the class comment of the included manifest class. The manifest class also includes other additional metadata for the package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : 'ManifestDrTestsCommentsToTests',
	#superclass : 'PackageManifest',
	#category : 'DrTests-CommentsToTests-Manifest',
	#package : 'DrTests-CommentsToTests',
	#tag : 'Manifest'
}

{ #category : 'meta-data - dependency analyser' }
ManifestDrTestsCommentsToTests class >> manuallyResolvedDependencies [

	<ignoreForCoverage>
	^ #(#'SUnit-UI')
]
