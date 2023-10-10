"
Please describe the package using the class comment of the included manifest class. The manifest class also includes other additional metadata for the package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : 'ManifestBaselineOfBaseLibraries',
	#superclass : 'PackageManifest',
	#category : 'BaselineOfBaseLibraries-Manifest',
	#package : 'BaselineOfBaseLibraries',
	#tag : 'Manifest'
}

{ #category : 'code-critics' }
ManifestBaselineOfBaseLibraries class >> ruleClassNotReferencedRuleV1FalsePositive [

	<ignoreForCoverage>
	^ #(#(#(#RGClassDefinition #(#BaselineOfBaseLibraries)) #'2023-04-06T20:47:21.36275+02:00') )
]
