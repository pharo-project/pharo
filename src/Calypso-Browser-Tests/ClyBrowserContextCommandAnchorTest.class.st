"
Tests for ClyBrowserContext>>#isDescribedByCommandAnchor:, which decides which command maps apply to a browser context: the ones of its class and superclasses, and the ones of the types of its selected items.
"
Class {
	#name : 'ClyBrowserContextCommandAnchorTest',
	#superclass : 'TestCase',
	#category : 'Calypso-Browser-Tests',
	#package : 'Calypso-Browser-Tests'
}

{ #category : 'helpers' }
ClyBrowserContextCommandAnchorTest >> methodContext [

	^ ClyFullBrowserMethodContext new
		  selectedItems: { (ClyDataSourceItem of: nil value: (ClyBrowserItem named: #= with: Object >> #=)) };
		  yourself
]

{ #category : 'tests' }
ClyBrowserContextCommandAnchorTest >> testIsDescribedByItsClassAndSuperclasses [

	self assert: (self methodContext isDescribedByCommandAnchor: ClyFullBrowserMethodContext).
	self assert: (self methodContext isDescribedByCommandAnchor: ClyFullBrowserContext).
	self deny: (self methodContext isDescribedByCommandAnchor: ClyFullBrowserClassContext)
]

{ #category : 'tests' }
ClyBrowserContextCommandAnchorTest >> testIsDescribedByTheTypeOfSelectedItems [

	self assert: (self methodContext isDescribedByCommandAnchor: ClyMethod).
	self deny: (self methodContext isDescribedByCommandAnchor: ClyClass)
]
