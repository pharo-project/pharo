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
	^ #(#(#(#RGMethodDefinition #(#ClassFactoryForTestCase #newSubclassOf:uses:instanceVariableNames:classVariableNames:category: #false)) #'2021-12-05T23:11:40.799968+01:00') #(#(#RGMethodDefinition #(#ClassFactoryForTestCase #newSubclassOf:uses:instanceVariableNames:classVariableNames:poolDictionaries:category: #false)) #'2021-12-05T23:12:15.813959+01:00') #(#(#RGMethodDefinition #(#ClassFactoryForTestCase #redefineClass:subclassOf:uses:instanceVariableNames:classVariableNames:poolDictionaries:package: #false)) #'2021-12-05T23:13:13.110809+01:00') )
]

{ #category : #'code-critics' }
ManifestSUnitCore class >> ruleStringConcatenationRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#ClassFactoryForTestCase #cleanUpChangeSetForClassNames: #false)) #'2021-12-05T23:07:46.009058+01:00') )
]

{ #category : #'code-critics' }
ManifestSUnitCore class >> ruleUtilityMethodsRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#ClassTestCase #categoriesForClass: #false)) #'2021-12-05T23:37:26.237093+01:00') )
]
