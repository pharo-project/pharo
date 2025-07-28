"
Please describe the package using the class comment of the included manifest class. The manifest class also includes other additional metadata for the package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : #ManifestZodiacTests,
	#superclass : #PackageManifest,
	#category : #'Zodiac-Tests-Manifest'
}

{ #category : #'code-critics' }
ManifestZodiacTests class >> ruleEmptyExceptionHandlerRuleV1FalsePositive [

	<ignoreForCoverage>
	^ #(#(#(#RGMethodDefinition #(#ZdcPluginSSLSessionFinalizationTest #tearDown #false)) #'2025-07-28T11:27:10.661654+02:00') )
]

{ #category : #'code-critics' }
ManifestZodiacTests class >> ruleLiteralArrayContainsSuspiciousTrueFalseOrNilRuleV1FalsePositive [

	<ignoreForCoverage>
	^ #(#(#(#RGPackageDefinition #(#'Zodiac-Tests')) #'2025-07-28T11:27:56.910159+02:00') )
]

{ #category : #'code-critics' }
ManifestZodiacTests class >> ruleReClassNotCategorizedRuleV1FalsePositive [

	<ignoreForCoverage>
	^ #(#(#(#RGPackageDefinition #(#'Zodiac-Tests')) #'2025-07-28T11:27:40.582327+02:00') )
]
