"
Core package for the SUnit unit testing framework
"
Class {
	#name : #ManifestSUnitCore,
	#superclass : #PackageManifest,
	#category : #'SUnit-Core-Manifest'
}

{ #category : #'code-critics' }
ManifestSUnitCore class >> ruleStringConcatenationRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#ClassFactoryForTestCase #cleanUpChangeSetForClassNames: #false)) #'2021-12-05T23:07:46.009058+01:00') )
]
