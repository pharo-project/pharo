"
Please describe the package using the class comment of the included manifest class. The manifest class also includes other additional metadata for the package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : 'ManifestZodiacTests',
	#superclass : 'PackageManifest',
	#category : 'Zodiac-Tests-Manifest',
	#package : 'Zodiac-Tests',
	#tag : 'Manifest'
}

{ #category : 'code-critics' }
ManifestZodiacTests class >> ruleEmptyExceptionHandlerRuleV1FalsePositive [

	<ignoreForCoverage>
	^ #(#(#(#RGPackageDefinition #(#'Zodiac-Tests')) #'2025-07-28T11:49:52.435454+02:00') )
]

{ #category : 'code-critics' }
ManifestZodiacTests class >> ruleReClassNotCategorizedRuleV1FalsePositive [

	<ignoreForCoverage>
	^ #(#(#(#RGPackageDefinition #(#'Zodiac-Tests')) #'2025-07-28T11:50:25.737818+02:00') )
]
