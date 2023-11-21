"
I store metadata for this package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : 'ManifestKernelTraits',
	#superclass : 'PackageManifest',
	#category : 'Kernel-Traits-Manifest',
	#package : 'Kernel-Traits',
	#tag : 'Manifest'
}

{ #category : 'code-critics' }
ManifestKernelTraits class >> ruleBetweenAndRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#TComparable #between:and: #false)) #'2020-05-02T12:57:07.451697+02:00') )
]

{ #category : 'code-critics' }
ManifestKernelTraits class >> ruleMinMaxRuleV1FalsePositive [

	<ignoreForCoverage>
	^ #(#(#(#RGMethodDefinition #(#TComparable #min: #false)) #'2023-11-20T22:41:18.539887+01:00') )
]

{ #category : 'code-critics' }
ManifestKernelTraits class >> ruleNotEliminationRuleV1FalsePositive [

	<ignoreForCoverage>
	^ #(#(#(#RGMethodDefinition #(#TComparable #'<=' #false)) #'2020-05-02T12:57:13.088697+02:00') #(#(#RGMethodDefinition #(#TIsEmpty #isNotEmpty #false)) #'2023-11-20T22:38:50.444209+01:00') )
]
