"
SUnit tests for monticello
"
Class {
	#name : #RGMonticelloTest,
	#superclass : #TestCase,
	#category : 'Ring-Tests-Monticello'
}

{ #category : #testing }
RGMonticelloTest >> testAsMCMethodDefinition [
	
	| ringMethod mcMethod |
	
	ringMethod := (OrderedCollection>>#size) asActiveRingDefinition.
	mcMethod := ringMethod asMCMethodDefinition.
	
	self assert: ringMethod = mcMethod asRingDefinition.
	self assert: mcMethod className = #OrderedCollection.
	self deny: mcMethod classIsMeta.
	self assert: mcMethod selector = #size.
	self assert: mcMethod category = #accessing.
	self assert: mcMethod source = (OrderedCollection>>#size) sourceCode
]

{ #category : #testing }
RGMonticelloTest >> testConvertingMCClassDefinition [
	| mcClass ringClass |
	
	mcClass := OrderedCollection asClassDefinition.
	ringClass := mcClass asRingDefinition.
	
	self assert: (ringClass isClass).
	self assert: (ringClass theMetaClass realClass = OrderedCollection class).
	self assert: (ringClass isSameRevisionAs: OrderedCollection asRingDefinition).
	self deny: (ringClass isSameRevisionAs: SortedCollection asRingDefinition)
]

{ #category : #testing }
RGMonticelloTest >> testConvertingMCMethodDefinition [
	| mcMethod ringMethod |
	
	ringMethod := (OrderedCollection >> #add: ) asRingDefinition.
	mcMethod := ringMethod asMCMethodDefinition.
	
	self assert: (ringMethod isMethod).
	self assert: (ringMethod isSameRevisionAs: ((OrderedCollection >> #add: ) asRingDefinition)).
	self deny: (ringMethod isSameRevisionAs: ((OrderedCollection >> #addAll: ) asRingDefinition))
]

{ #category : #testing }
RGMonticelloTest >> testConvertingMCOrganizationDefinition [
	| mcOrganization ringOrganization mcOrganization2 |
	
	mcOrganization := MCOrganizationDefinition categories: {#'Ring-Core-Kernel' . #'Ring-Core-Definitions'}.
	mcOrganization2 := MCOrganizationDefinition categories: {#'Ring-Core-Kernel' . #'Ring-Core-Definitions' . #'Ring-Core-Variables'}.
	ringOrganization := mcOrganization asRingDefinition.
	
	self assert: (ringOrganization isOrganization).
	self deny: (ringOrganization isSameRevisionAs: mcOrganization2 asRingDefinition)
]

{ #category : #testing }
RGMonticelloTest >> testConvertingMCTraitDefinition [
	| mcClass ringClass |
	
	mcClass := TSortable asClassDefinition.
	ringClass := mcClass asRingDefinition.
	
	self assert: (ringClass isTrait).
	self assert: (ringClass theMetaClass realClass = TSortable theMetaClass).
	self assert: (ringClass isSameRevisionAs: TSortable asRingDefinition).
	self deny: (ringClass isSameRevisionAs: TBehavior asRingDefinition)
]
