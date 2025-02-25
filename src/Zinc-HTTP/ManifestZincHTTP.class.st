"
Please describe the package using the class comment of the included manifest class. The manifest class also includes other additional metadata for the package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : 'ManifestZincHTTP',
	#superclass : 'PackageManifest',
	#category : 'Zinc-HTTP-Manifest',
	#package : 'Zinc-HTTP',
	#tag : 'Manifest'
}

{ #category : 'code-critics' }
ManifestZincHTTP class >> ruleNotEliminationRuleV1FalsePositive [

	<ignoreForCoverage>
	^ #(#(#(#RGMethodDefinition #(#ZnMimePart #hasHeaders #false)) #'2024-03-23T16:05:34.582387+01:00') )
]
