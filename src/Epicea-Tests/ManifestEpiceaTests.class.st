"
Please describe the package using the class comment of the included manifest class. The manifest class also includes other additional metadata for the package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : 'ManifestEpiceaTests',
	#superclass : 'PackageManifest',
	#category : 'Epicea-Tests-Manifest',
	#package : 'Epicea-Tests',
	#tag : 'Manifest'
}

{ #category : 'code-critics' }
ManifestEpiceaTests class >> ruleEmptyExceptionHandlerRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#EpDisabledIntegrationTest #testDisableDuringWithInterruption #false)) #'2021-11-27T00:09:09.223669+01:00') )
]
