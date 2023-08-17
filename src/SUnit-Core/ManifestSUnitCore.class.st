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

	<ignoreForCoverage>
	^ #(#(#(#RGMethodDefinition #(#ClassFactoryForTestCase #cleanUpChangeSetForClasses: #false)) #'2023-08-17T13:09:48.43551+02:00') )
]
