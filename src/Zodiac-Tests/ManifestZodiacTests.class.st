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
	^ #(#(#(#RGMethodDefinition #(#ZdcPluginSSLSessionFinalizationTest #tearDown #false)) #'2025-07-28T11:33:08.113066+02:00') )
]

{ #category : 'code-critics' }
ManifestZodiacTests class >> ruleReClassNotCategorizedRuleV1FalsePositive [

	<ignoreForCoverage>
	^ #(#(#(#RGPackageDefinition #(#'Zodiac-Tests')) #'2025-07-28T11:33:19.100601+02:00') )
]
