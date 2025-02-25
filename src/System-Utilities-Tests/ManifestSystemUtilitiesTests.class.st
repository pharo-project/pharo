"
Please describe the package using the class comment of the included manifest class. The manifest class also includes other additional metadata for the package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : 'ManifestSystemUtilitiesTests',
	#superclass : 'PackageManifest',
	#category : 'System-Utilities-Tests-Manifest',
	#package : 'System-Utilities-Tests',
	#tag : 'Manifest'
}

{ #category : 'code-critics' }
ManifestSystemUtilitiesTests class >> ruleStringConcatenationRuleV1FalsePositive [

	<ignoreForCoverage>
	^ #(#(#(#RGMethodDefinition #(#ClipboardTest #testRecentClippingIsLIFOAndRotating #false)) #'2023-11-21T22:11:16.858693+01:00') )
]
