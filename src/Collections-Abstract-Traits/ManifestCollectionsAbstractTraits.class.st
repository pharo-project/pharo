"
I store metadata for this package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : #ManifestCollectionsAbstractTraits,
	#superclass : #PackageManifest,
	#category : #'Collections-Abstract-Traits-Manifest'
}

{ #category : #'code-critics' }
ManifestCollectionsAbstractTraits class >> ruleExcessiveArgumentsRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#TSortable #mergeFirst:middle:last:into:by: #false)) #'2020-06-26T09:33:14.853351+02:00') #(#(#RGMethodDefinition #(#TSortable #mergeSortFrom:to:src:dst:by: #false)) #'2020-06-26T09:33:26.566053+02:00') )
]
