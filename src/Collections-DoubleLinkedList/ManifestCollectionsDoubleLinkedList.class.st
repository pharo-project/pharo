"
I store metadata for this package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : 'ManifestCollectionsDoubleLinkedList',
	#superclass : 'PackageManifest',
	#category : 'Collections-DoubleLinkedList-Manifest',
	#package : 'Collections-DoubleLinkedList',
	#tag : 'Manifest'
}

{ #category : 'code-critics' }
ManifestCollectionsDoubleLinkedList class >> ruleCollectionProtocolRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#DoubleLinkedList #addAll: #false)) #'2020-11-13T21:03:12.373876+01:00') #(#(#RGMethodDefinition #(#DoubleLinkedList #collect: #false)) #'2020-11-13T21:03:27.372294+01:00') )
]

{ #category : 'code-critics' }
ManifestCollectionsDoubleLinkedList class >> ruleLiteralArrayContainsSuspiciousTrueFalseOrNilRuleV1FalsePositive [
	^ #(#(#(#RGMetaclassDefinition #(#'ManifestCollectionsDoubleLinkedList class' #ManifestCollectionsDoubleLinkedList)) #'2022-12-30T04:35:42.465732+01:00') )
]

{ #category : 'code-critics' }
ManifestCollectionsDoubleLinkedList class >> ruleNotEliminationRuleV1FalsePositive [
	^ #(#(#(#RGMethodDefinition #(#DoubleLinkedList #isNotEmpty #false)) #'2022-12-30T04:35:00.58632+01:00') )
]
