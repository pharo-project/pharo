"
Please describe the package using the class comment of the included manifest class. The manifest class also includes other additional metadata for the package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : 'ManifestBaselineOfPharo',
	#superclass : 'PackageManifest',
	#category : 'BaselineOfPharo-Manifest',
	#package : 'BaselineOfPharo',
	#tag : 'Manifest'
}

{ #category : 'code-critics' }
ManifestBaselineOfPharo class >> ruleClassNotReferencedRuleV1FalsePositive [

	<ignoreForCoverage>
	^ #(#(#(#RGClassDefinition #(#BaselineOfPharo)) #'2023-11-21T08:59:20.720605+01:00') )
]
