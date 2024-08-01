"
Please describe the package using the class comment of the included manifest class. The manifest class also includes other additional metadata for the package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : 'ManifestGeneralRules',
	#superclass : 'PackageManifest',
	#category : 'General-Rules-Manifest',
	#package : 'General-Rules',
	#tag : 'Manifest'
}

{ #category : 'code-critics' }
ManifestGeneralRules class >> ruleBadMessageRule2V1FalsePositive [

	<ignoreForCoverage>
	^ #(#(#(#RGMethodDefinition #(#ReBaselineWithProperSuperclassRule #basicCheck: #false)) #'2024-05-13T22:11:20.009568+02:00') )
]

{ #category : 'code-critics' }
ManifestGeneralRules class >> ruleLiteralArrayCharactersRuleV1FalsePositive [

	<ignoreForCoverage>
	^ #(#(#(#RGMethodDefinition #(#ReMethodSelectorKeywordCasingRule #basicCheck: #false)) #'2024-02-19T21:54:28.482657+01:00') )
]
