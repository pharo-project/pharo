"
Please describe the package using the class comment of the included manifest class. The manifest class also includes other additional metadata for the package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : 'ManifestBaselineOfIDE',
	#superclass : 'PackageManifest',
	#category : 'BaselineOfIDE-Manifest',
	#package : 'BaselineOfIDE',
	#tag : 'Manifest'
}

{ #category : 'code-critics' }
ManifestBaselineOfIDE class >> ruleLongMethodsRuleV1FalsePositive [

	<ignoreForCoverage>
	^ #(#(#(#RGMethodDefinition #(#BaselineOfIDE #baseline: #false)) #'2023-11-21T08:59:08.032603+01:00') )
]
