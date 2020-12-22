"
I store metadata for this package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : #ManifestCollectionsDoubleLinkedList,
	#superclass : #PackageManifest,
	#category : #'Collections-DoubleLinkedList-Manifest'
}

{ #category : #'code-critics' }
ManifestCollectionsDoubleLinkedList class >> ruleCollectionProtocolRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#DoubleLinkedList #addAll: #false)) #'2020-11-13T21:03:12.373876+01:00') #(#(#RGMethodDefinition #(#DoubleLinkedList #collect: #false)) #'2020-11-13T21:03:27.372294+01:00') )
]
