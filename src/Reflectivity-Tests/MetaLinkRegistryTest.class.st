Class {
	#name : #MetaLinkRegistryTest,
	#superclass : #TestCase,
	#instVars : [
		'registry',
		'permalink'
	],
	#category : #'Reflectivity-Tests-Base'
}

{ #category : #running }
MetaLinkRegistryTest >> setUp [

	super setUp.

	registry := MetaLinkRegistry new.
	permalink := PermaLink new.
	permalink slotOrVarClass: ReflectivityExamples2.
	permalink slotOrVariable: (ReflectivityExamples2 lookupVar: #instVar2).
	registry registerPermaLink: permalink.
	registry slotSourcesAt: ReflectivityExamples2 ifAbsentPut: [ 
		Set new
			add: permalink slotOrVariable;
			yourself ]
]

{ #category : #tests }
MetaLinkRegistryTest >> testPermaLinksForMethod [
	|plinks|
	plinks := registry permaLinksForMethod: ReflectivityExamples2Subclass>>#useInstVarInSubclass.
	self assert: plinks size equals: 1.
	self assert: plinks first identicalTo: permalink 
]

{ #category : #tests }
MetaLinkRegistryTest >> testSlotSourcesConsiderClassesHierarchy [

	| slotSources |
	slotSources := (registry
		                slotSourcesAt: ReflectivityExamples2Subclass
		                ifAbsent: [ nil ]) flattened.
	self assert: slotSources size equals: 1.
	self
		assert: slotSources asArray first
		identicalTo: permalink slotOrVariable 
]
