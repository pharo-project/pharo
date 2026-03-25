"
Please describe the package using the class comment of the included manifest class. The manifest class also includes other additional metadata for the package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : 'ManifestRubric',
	#superclass : 'PackageManifest',
	#category : 'Rubric-Manifest',
	#package : 'Rubric',
	#tag : 'Manifest'
}

{ #category : 'meta-data - dependency analyser' }
ManifestRubric class >> manuallyResolvedDependencies [

	<ignoreForCoverage>
	^ #(#'System-Localization' #'Transcript-NonInteractive' #'Text-Edition')
]
