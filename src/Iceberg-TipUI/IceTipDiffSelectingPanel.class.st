"
I'm a diff panel that shows packages/classes/methods changed in a column and a diff in the other.
I give users the possibility of select changes.  

By default, all my elements are selected on opening.

As my father, I receive a model that needs to understand #elements.
also #elements answer should be a collection of elements that also understand elements.


"
Class {
	#name : #IceTipDiffSelectingPanel,
	#superclass : #IceTipDiffPanel,
	#category : #'Iceberg-TipUI-View'
}

{ #category : #private }
IceTipDiffSelectingPanel >> entryRemoved: ann [
	self refresh
]

{ #category : #initialize }
IceTipDiffSelectingPanel >> initialize [
	super initialize.
	self subscribeToAnnouncements
]

{ #category : #'private factory' }
IceTipDiffSelectingPanel >> newChangeTreeDataSource [
	^ IceTipTreeSelectingDataSource new
		tool: self;
		rootForItems: self model treeRoots;
		childrenBlock: [ :data | data children ];
		icon: [ :each | each value icon ];
		format: #description;
		selectAll;
		yourself
]

{ #category : #accessing }
IceTipDiffSelectingPanel >> refresh [
	self model reset.
	self resetDiffContents.
	changeList widget 
		in: [ :this |
			this basicSelectRowIndexes: #(). 
			this dataSource 
				rootForItems: self model treeRoots;
				selectAll;
				expandAll ];
		refresh
]

{ #category : #private }
IceTipDiffSelectingPanel >> resetDataSource: aDataSource [
	super resetDataSource: aDataSource.
	aDataSource selectAll
]

{ #category : #accessing }
IceTipDiffSelectingPanel >> selectedItems [
	^ changeList widget dataSource selectedItems
]

{ #category : #initialize }
IceTipDiffSelectingPanel >> subscribeToAnnouncements [
	self announcer
		when: IceTipDiffEntryRemoved 
		send: #entryRemoved:
		to: self	
]
