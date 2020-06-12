"
I store metadata for this package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : #ManifestCollectionsDoubleLinkedListTests,
	#superclass : #PackageManifest,
	#category : #'Collections-DoubleLinkedList-Tests-Manifest'
}

{ #category : #'code-critics' }
ManifestCollectionsDoubleLinkedListTests class >> ruleReUsesAddRuleV1FalsePositive [
	^ #(#(#(#RGClassDefinition #(#DoubleLinkedListTest)) #'2020-03-27T12:31:42.565385+01:00') )
]

{ #category : #'code-critics' }
ManifestCollectionsDoubleLinkedListTests class >> ruleUsesAddRuleV1FalsePositive [
	^ #(#(#(#RGClassDefinition #(#DoubleLinkedListTest)) #'2020-06-12T14:46:39.462891+02:00') )
]
