"
Core package for the SUnit unit testing framework
"
Class {
	#name : #ManifestSUnitCore,
	#superclass : #PackageManifest,
	#category : #'SUnit-Core-Manifest'
}

{ #category : #'code-critics' }
ManifestSUnitCore class >> ruleExcessiveArgumentsRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#ClassFactoryForTestCase #redefineClass:subclassOf:uses:instanceVariableNames:classVariableNames:poolDictionaries:package: #false)) #'2021-12-05T23:13:13.110809+01:00') )
]

{ #category : #'code-critics' }
ManifestSUnitCore class >> ruleStringConcatenationRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#ClassFactoryForTestCase #cleanUpChangeSetForClassNames: #false)) #'2021-12-05T23:07:46.009058+01:00') )
]
