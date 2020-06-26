"
I store metadata for this package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : #ManifestKernelTraits,
	#superclass : #PackageManifest,
	#category : #'Kernel-Traits-Manifest'
}

{ #category : #'code-critics' }
ManifestKernelTraits class >> ruleBetweenAndRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#TComparable #between:and: #false)) #'2020-05-02T12:57:07.451697+02:00') )
]

{ #category : #'code-critics' }
ManifestKernelTraits class >> ruleNotEliminationRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#TComparable #'<=' #false)) #'2020-05-02T12:57:13.088697+02:00') )
]
