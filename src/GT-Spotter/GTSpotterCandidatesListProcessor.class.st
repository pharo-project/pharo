"
This is a candidates processor that can be parameterized via blocks.
"
Class {
	#name : #GTSpotterCandidatesListProcessor,
	#superclass : #GTSpotterCandidatesProcessor,
	#instVars : [
		'allCandidatesBlock',
		'candidatesLimit',
		'queryMatchingBlock',
		'title',
		'itemNameBlock',
		'itemIconBlock',
		'actBlock',
		'wantsToDisplayOnEmptyQuery',
		'filterBlock',
		'itemFilterNameBlock',
		'keyBinding',
		'sortBlock'
	],
	#category : #'GT-Spotter-Processors'
}

{ #category : #accessing }
GTSpotterCandidatesListProcessor >> actLogic [
	^ actBlock
]

{ #category : #scripting }
GTSpotterCandidatesListProcessor >> actLogic: aBlockWithTwoArguments [

	actBlock := aBlockWithTwoArguments
]

{ #category : #private }
GTSpotterCandidatesListProcessor >> actOn: anObject for: aStep [
	
	actBlock
		ifNil: [ super actOn: anObject for: aStep ]
		ifNotNil: [ actBlock cull: anObject cull: aStep ]
]

{ #category : #compatibility }
GTSpotterCandidatesListProcessor >> allCandidates: aBlockWithOneArgument [
	self items: aBlockWithOneArgument
]

{ #category : #compatibility }
GTSpotterCandidatesListProcessor >> candidatesLimit: anInteger [
	self itemsLimit: anInteger
]

{ #category : #private }
GTSpotterCandidatesListProcessor >> computeAllItemsIn: aContext [
	^ allCandidatesBlock cull: aContext
]

{ #category : #compatibility }
GTSpotterCandidatesListProcessor >> defaultCandidatesLimit [
	^ self defaultItemsLimit
]

{ #category : #'accessing-defaults' }
GTSpotterCandidatesListProcessor >> defaultContinueItemsLimit [
	" Spotter becomes extraordenary slow when rendering too many results. So we set the maximum limit to 100. This is only a temporary solution until we have fixed the problem. Until then no spotter processor will ever generate more then 100 results. "
	^ 100
]

{ #category : #'accessing-defaults' }
GTSpotterCandidatesListProcessor >> defaultItemsLimit [
	^ 5
]

{ #category : #private }
GTSpotterCandidatesListProcessor >> doFilterInContext: aSpotterContext [
	^ aSpotterContext doFilter: filterBlock gtFilter forProcessor: self
]

{ #category : #accessing }
GTSpotterCandidatesListProcessor >> filter [
	^ filterBlock
]

{ #category : #scripting }
GTSpotterCandidatesListProcessor >> filter: aGTFilterOrBlockWithOneArgument [
	filterBlock := aGTFilterOrBlockWithOneArgument 
]

{ #category : #scripting }
GTSpotterCandidatesListProcessor >> filter: aGTFilter item: aGTSpotterIterator [ 
	self filter: aGTFilter.
	self items: aGTSpotterIterator gtIterator.
	
]

{ #category : #compatibility }
GTSpotterCandidatesListProcessor >> filterUsing: aFilter [
	self filter: aFilter
]

{ #category : #testing }
GTSpotterCandidatesListProcessor >> hasDynamicItems [
	^ allCandidatesBlock hasDynamicItems
]

{ #category : #accessing }
GTSpotterCandidatesListProcessor >> itemFilterName [
	^ itemFilterNameBlock
]

{ #category : #scripting }
GTSpotterCandidatesListProcessor >> itemFilterName: aBlockWithOneArgument [
	itemFilterNameBlock := aBlockWithOneArgument
]

{ #category : #private }
GTSpotterCandidatesListProcessor >> itemFilterNameFor: anObject [
	^ itemFilterNameBlock isNil
		ifTrue: [ super itemFilterNameFor: anObject ]
		ifFalse: [ itemFilterNameBlock cull: anObject ]
]

{ #category : #accessing }
GTSpotterCandidatesListProcessor >> itemIcon [
	^ itemIconBlock
]

{ #category : #scripting }
GTSpotterCandidatesListProcessor >> itemIcon: aBlockWithOneArgument [
	itemIconBlock := aBlockWithOneArgument
]

{ #category : #private }
GTSpotterCandidatesListProcessor >> itemIconFor: anObject [
	^ itemIconBlock isNil
		ifTrue: [ super itemIconFor: anObject ]
		ifFalse: [ itemIconBlock cull: anObject ]
]

{ #category : #accessing }
GTSpotterCandidatesListProcessor >> itemName [
	^ itemNameBlock
]

{ #category : #scripting }
GTSpotterCandidatesListProcessor >> itemName: aBlockWithOneArgument [
	itemNameBlock := aBlockWithOneArgument
]

{ #category : #private }
GTSpotterCandidatesListProcessor >> itemNameFor: anObject [
	^ itemNameBlock isNil
		ifTrue: [ super itemNameFor: anObject ]
		ifFalse: [ itemNameBlock cull: anObject ]
]

{ #category : #scripting }
GTSpotterCandidatesListProcessor >> items: aBlock [
	allCandidatesBlock := aBlock
]

{ #category : #accessing }
GTSpotterCandidatesListProcessor >> itemsLimit [
	^ candidatesLimit ifNil: [ self defaultItemsLimit ]
]

{ #category : #scripting }
GTSpotterCandidatesListProcessor >> itemsLimit: anInteger [
	candidatesLimit := anInteger
]

{ #category : #accessing }
GTSpotterCandidatesListProcessor >> keyBinding [
	^ keyBinding
]

{ #category : #accessing }
GTSpotterCandidatesListProcessor >> keyBinding: anObject [
	keyBinding := anObject
]

{ #category : #'scripting-convenience' }
GTSpotterCandidatesListProcessor >> match: aBlockWithTwoArguments [
	self filter: (GTFilterBlock new 
		matcher: aBlockWithTwoArguments;
		yourself)
]

{ #category : #'scripting-convenience' }
GTSpotterCandidatesListProcessor >> matchAlike [
	self filter: GTFilterAlike
]

{ #category : #'scripting-convenience' }
GTSpotterCandidatesListProcessor >> matchRegex [
	self filter: GTFilterRegex
]

{ #category : #'scripting-convenience' }
GTSpotterCandidatesListProcessor >> matchString [
	self filter: GTFilterStringMatch
]

{ #category : #'scripting-convenience' }
GTSpotterCandidatesListProcessor >> matchSubstring [
	self filter: GTFilterSubstring
]

{ #category : #'scripting-convenience' }
GTSpotterCandidatesListProcessor >> matchSubstrings [
	self filter: GTFilterSubstrings
]

{ #category : #accessing }
GTSpotterCandidatesListProcessor >> sort [
	^ sortBlock
]

{ #category : #scripting }
GTSpotterCandidatesListProcessor >> sort: aBlockWithTwoArguments [
	sortBlock := aBlockWithTwoArguments
]

{ #category : #accessing }
GTSpotterCandidatesListProcessor >> title [
	^ title ifNil: [ super title ]
]

{ #category : #scripting }
GTSpotterCandidatesListProcessor >> title: aString [
	title := aString
]

{ #category : #accessing }
GTSpotterCandidatesListProcessor >> wantsToDisplayOnEmptyQuery [
	^ wantsToDisplayOnEmptyQuery ifNil: [ super wantsToDisplayOnEmptyQuery ]
]

{ #category : #scripting }
GTSpotterCandidatesListProcessor >> wantsToDisplayOnEmptyQuery: anObject [
	wantsToDisplayOnEmptyQuery := anObject
]

{ #category : #private }
GTSpotterCandidatesListProcessor >> withItemsLimit: aLimit do: aBlock [
	| previousCandidatesLimit |
	previousCandidatesLimit := candidatesLimit.
	^ [ candidatesLimit := aLimit. aBlock value ] 
		ensure: [ candidatesLimit := previousCandidatesLimit ]
]
