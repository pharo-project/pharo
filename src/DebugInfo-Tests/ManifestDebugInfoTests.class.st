"
Please describe the package using the class comment of the included manifest class. The manifest class also includes other additional metadata for the package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : 'ManifestDebugInfoTests',
	#superclass : 'PackageManifest',
	#category : 'DebugInfo-Tests-Manifest',
	#package : 'DebugInfo-Tests',
	#tag : 'Manifest'
}

{ #category : 'code-critics' }
ManifestDebugInfoTests class >> ruleUseSetUpRuleV1FalsePositive [

	<ignoreForCoverage>
	^ #(#(#(#RGPackageDefinition #(#'DebugInfo-Tests')) #'2025-09-10T14:10:08.434728+02:00') )
]
