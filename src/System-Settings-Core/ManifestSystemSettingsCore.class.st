"
Please describe the package using the class comment of the included manifest class. The manifest class also includes other additional metadata for the package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : 'ManifestSystemSettingsCore',
	#superclass : 'PackageManifest',
	#category : 'System-Settings-Core-Manifest',
	#package : 'System-Settings-Core',
	#tag : 'Manifest'
}

{ #category : 'meta-data - dependency analyser' }
ManifestSystemSettingsCore class >> manuallyResolvedDependencies [

	<ignoreForCoverage>
	^ #(#'System-Localization' #'System-Object Events')
]
