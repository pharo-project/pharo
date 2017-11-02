Class {
	#name : #EpFilterTest,
	#superclass : #TestCase,
	#category : 'EpiceaBrowsersTests-Filters'
}

{ #category : #running }
EpFilterTest >> assert: aFilter accepts: anEvent rejects: anotherEvent [
	self assert: (aFilter accepts: anEvent).
	self deny: (aFilter accepts: anotherEvent).
]

{ #category : #resources }
EpFilterTest >> classAddition [
	^ self entryWith: (EpClassAddition class: Object)
]

{ #category : #resources }
EpFilterTest >> diverseEvents [
	^ {
		self classAddition.
		self expressionEvaluation.
		self methodAddition.
		self redo.
		self refactoring.
		}
]

{ #category : #running }
EpFilterTest >> entryWith: anEvent [
	^ OmEntry content: anEvent
]

{ #category : #resources }
EpFilterTest >> expressionEvaluation [
	^ self entryWith: (EpExpressionEvaluation expression: '42')
]

{ #category : #resources }
EpFilterTest >> methodAddition [
	^ self entryWith: (EpMethodAddition method: Object >> #printOn:)
]

{ #category : #resources }
EpFilterTest >> redo [
	^ self entryWith: (EpRedo forAll: #())
]

{ #category : #resources }
EpFilterTest >> refactoring [
	^ self entryWith: (EpRenameClassRefactoring rename: Object to: #Object42)
]

{ #category : #tests }
EpFilterTest >> testAnd [
	| aFilter |
	aFilter := EpAndFilter withAll: { 
		EpPluggableFilter codeChangeFilter. 
		EpPluggableFilter methodFilter }.
	
	self 
		assert: aFilter
		accepts: self methodAddition
		rejects: self classAddition
]

{ #category : #tests }
EpFilterTest >> testEquality [
	self 
		assert: EpPluggableFilter methodFilter
		equals: EpPluggableFilter methodFilter.
	
	self 
		assert: (EpOrFilter withAll: EpPluggableFilter allFilters)
		equals: (EpOrFilter withAll: EpPluggableFilter allFilters).
]

{ #category : #tests }
EpFilterTest >> testLogBrowserOperation [
	self
		assert: EpPluggableFilter logBrowserOperationFilter
		accepts: self redo
		rejects: self methodAddition
]

{ #category : #tests }
EpFilterTest >> testMethod [	
	self 
		assert: EpPluggableFilter methodFilter
		accepts: self methodAddition
		rejects: self classAddition
]

{ #category : #tests }
EpFilterTest >> testNoTrigger [
	| expressionEvaluationWithTrigger |
	expressionEvaluationWithTrigger := self classAddition.
	expressionEvaluationWithTrigger tags 
		at: EpLog triggererReferenceKey 
		put: (OmReference globalName: 'test' localName: '1').
				
	self
		assert: EpPluggableFilter noTriggerFilter
		accepts: self classAddition
		rejects: expressionEvaluationWithTrigger
]
