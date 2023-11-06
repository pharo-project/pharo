"
Please describe the package using the class comment of the included manifest class. The manifest class also includes other additional metadata for the package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : 'ManifestRenrakuTests',
	#superclass : 'PackageManifest',
	#category : 'Renraku-Tests-Manifest',
	#package : 'Renraku-Tests',
	#tag : 'Manifest'
}

{ #category : 'code-critics' }
ManifestRenrakuTests class >> ruleLiteralArrayContainsSuspiciousTrueFalseOrNilRuleV1FalsePositive [
	^ #(#(#(#RGMetaclassDefinition #(#'ManifestRenrakuTests class' #ManifestRenrakuTests)) #'2023-01-29T17:30:38.57322+01:00') )
]

{ #category : 'code-critics' }
ManifestRenrakuTests class >> ruleReCompactSourceCodeRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#ReMethodSourceCleanerTest #properMethodWithFinalDot #false)) #'2023-01-29T17:28:55.55396+01:00') )
]

{ #category : 'code-critics' }
ManifestRenrakuTests class >> ruleRefersToClassRuleV1FalsePositive [
	^ #(#(#(#RGClassDefinition #(#ReMethodSourceCleanerTest)) #'2023-01-27T22:50:46.104645+01:00') )
]
