"
Includes unit tests for double linked list package
"
Class {
	#name : #ManifestCollectionsDoubleLinkedListTests,
	#superclass : #PackageManifest,
	#category : #'Collections-DoubleLinkedList-Tests-Manifest'
}

{ #category : #'code-critics' }
ManifestCollectionsDoubleLinkedListTests class >> ruleRBUsesAddRuleV1FalsePositive [
	^ #(#(#(#RGClassDefinition #(#DoubleLinkedListTests)) #'2019-01-28T20:31:10.042921+01:00') )
]
