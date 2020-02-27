"
SUnit tests for monticello
"
Class {
	#name : #RGMonticelloTest,
	#superclass : #TestCase,
	#category : #'Ring-Definitions-Tests-Monticello-Base'
}

{ #category : #testing }
RGMonticelloTest >> testAsMCMethodDefinition [
	| ringMethod mcMethod |
	ringMethod := (OrderedCollection >> #size) asActiveRingDefinition.
	mcMethod := ringMethod asMCMethodDefinition.

	self assert: ringMethod equals: mcMethod asRingDefinition.
	self assert: mcMethod className equals: #OrderedCollection.
	self deny: mcMethod classIsMeta.
	self assert: mcMethod selector equals: #size.
	self assert: mcMethod category equals: #accessing.
	self assert: mcMethod source equals: (OrderedCollection >> #size) sourceCode
]

{ #category : #testing }
RGMonticelloTest >> testConvertingMCClassDefinition [
	| mcClass ringClass |
	mcClass := OrderedCollection asClassDefinition.
	ringClass := mcClass asRingDefinition.

	self assert: ringClass isClass.
	self assert: ringClass classSide realClass equals: OrderedCollection class.
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
	
	mcOrganization := MCOrganizationDefinition categories: {#'Ring-Definitions-Core' . #'Ring-Definitions-Core-Definitions'}.
	mcOrganization2 := MCOrganizationDefinition categories: {#'Ring-Definitions-Core' . #'Ring-Definitions-Core-Definitions' . #'Ring-Definitions-Core-Variables'}.
	ringOrganization := mcOrganization asRingDefinition.
	
	self assert: (ringOrganization isOrganization).
	self deny: (ringOrganization isSameRevisionAs: mcOrganization2 asRingDefinition)
]

{ #category : #testing }
RGMonticelloTest >> testConvertingMCTraitDefinition [
	| mcClass ringClass |
	mcClass := TSortable asClassDefinition.
	ringClass := mcClass asRingDefinition.

	self assert: ringClass isTrait.
	self assert: ringClass classSide realClass equals: TSortable classSide.
	self assert: (ringClass isSameRevisionAs: TSortable asRingDefinition).
	self deny: (ringClass isSameRevisionAs: TAssertable asRingDefinition)
]
