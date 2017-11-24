"
This is the abstract class that models a processor that is associated with a spotter category. It can spot items to be displayed in that category by reacting to a filter: message. 
"
Class {
	#name : #GTSpotterProcessor,
	#superclass : #Object,
	#instVars : [
		'allFilteredCandidates',
		'order'
	],
	#category : #'GT-Spotter-Processors'
}

{ #category : #public }
GTSpotterProcessor >> actOn: anObject for: aStep [
	anObject spotterActDefault: aStep
]

{ #category : #public }
GTSpotterProcessor >> allFilteredCandidates [
	^ allFilteredCandidates ifNil: [ allFilteredCandidates := OrderedCollection new ]
]

{ #category : #private }
GTSpotterProcessor >> basicContinueFilterInContext: aSpotterContext [

	" We DO NOT prepare the context since we could no longer continue producing filtered candidates. "
	(self shouldFilterInContext: aSpotterContext) ifTrue: [
		" The continue-filter only puts more items on the stream but doesn't do any filtering at all. Therefore the original collection (=result/candidates) and its size remains the same and can be reused after the continue-filter has added more items. So we do not ressign the result to #allFilteredCandidates as it is done for the regular filter-loop. #allFilteredCandidates must never be modified more than once because the next #diveIntoCategory would render an invalid set of items !!! "
		self doContinueFilterInContext: aSpotterContext ].
	aSpotterContext stream 
		performSymbol: #onAmountChanged:in:
		withArguments: { self allFilteredCandidates size . self }
]

{ #category : #private }
GTSpotterProcessor >> basicFilterInContext: aSpotterContext [
	
	self prepareProcessorInContext: aSpotterContext.
	(self shouldFilterInContext: aSpotterContext) ifTrue: [
		allFilteredCandidates := self doFilterInContext: aSpotterContext ].
	aSpotterContext stream 
		performSymbol: #onAmountChanged:in: 
		withArguments: { self allFilteredCandidates size . self }
]

{ #category : #private }
GTSpotterProcessor >> continueFilterInContext: aSpotterContext [
	[ self basicContinueFilterInContext: aSpotterContext ]
		on: Error 
		do: [ :exception | 
			self exceptionHandler 
				handleException: exception 
				do: [ ('[Spotter] Exception in continuing processor <', self title asString, '>: ', exception asString) logCr ] ]
]

{ #category : #private }
GTSpotterProcessor >> doContinueFilterInContext: aSpotterContext [
	^ aSpotterContext doContinueFilterForProcessor: self
]

{ #category : #private }
GTSpotterProcessor >> doFilterInContext: aSpotterContext [
	"override this method to:
	- add items to the stream
	- return all found items at the end"
	self subclassResponsibility
]

{ #category : #'accessing-dynamic' }
GTSpotterProcessor >> exceptionHandler [
	self flag: 'Bloc -> overrides doesNotUnderstand discarding the current exception context'.
	^ GTCurrentSpotterExceptionHandler value ifNil: [ GTSpotterExceptionHandlerNotFound signal ]
]

{ #category : #private }
GTSpotterProcessor >> filterInContext: aSpotterContext [
	[ self basicFilterInContext: aSpotterContext ]
		on: Error 
		do: [ :exception | 
			self exceptionHandler 
				handleException: exception 
				do: [ ('[Spotter] Exception in processor <', self title asString, '>: ', exception asString) logCr ] ]
]

{ #category : #printing }
GTSpotterProcessor >> gtDisplayOn: stream [
	stream 
		nextPutAll: 'Processor: ';
		nextPutAll: self title asString
]

{ #category : #testing }
GTSpotterProcessor >> hasFilteredCandidates [
	^ self allFilteredCandidates notEmpty
]

{ #category : #accessing }
GTSpotterProcessor >> order [
	"Return assigned spotter order used to arrange categories in spotter UI"
	<return: #Number>
	
	^ order
]

{ #category : #accessing }
GTSpotterProcessor >> order: aNumber [
	"Assign to processor its spotter order to arrange categories
	within spotter ui"
	order := aNumber
]

{ #category : #private }
GTSpotterProcessor >> prepareProcessorInContext: aContext [
	allFilteredCandidates := nil
]

{ #category : #private }
GTSpotterProcessor >> render: anObject [
	^ GTSpotterItemBrick new
		icon: (nil);
		text: (anObject gtDisplayString)
]

{ #category : #testing }
GTSpotterProcessor >> shouldFilterInContext: aSpotterContext [
	^ aSpotterContext notEmpty 
		or: [ self wantsToDisplayOnEmptyQuery ]
]

{ #category : #accessing }
GTSpotterProcessor >> title [
	self subclassResponsibility
]

{ #category : #accessing }
GTSpotterProcessor >> wantsToDisplayOnEmptyQuery [
	^ true
]

{ #category : #private }
GTSpotterProcessor >> withItemsLimit: aLimit do: aBlock [
	^ aBlock value
]
