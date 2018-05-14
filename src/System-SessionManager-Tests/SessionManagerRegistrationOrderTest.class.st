"
SUnit tests for registration order on the Pharo session manager
"
Class {
	#name : #SessionManagerRegistrationOrderTest,
	#superclass : #TestCase,
	#instVars : [
		'manager'
	],
	#category : #'System-SessionManager-Tests'
}

{ #category : #asserting }
SessionManagerRegistrationOrderTest >> assertHandledId: aSessionHandlerList equals: aCollection [
	self 
		assert: (aSessionHandlerList collect: #handledId)
		equals: aCollection
]

{ #category : #tests }
SessionManagerRegistrationOrderTest >> register: aString atSystemPriority: anInteger [
	manager 
		register: aString
		inCategory: manager systemCategory
		atPriority: anInteger
]

{ #category : #running }
SessionManagerRegistrationOrderTest >> setUp [
	super setUp.
	manager := SessionManager new
]

{ #category : #tests }
SessionManagerRegistrationOrderTest >> testCategoriesPriorityOrder [
	manager 
		register: (ClassSessionHandler forClassNamed: 'anObject');
		registerSystemClassNamed: 'aFirstSubscription' atPriority: 100.
	
	self 
		assertHandledId: manager startupList 
		equals: #( 'aFirstSubscription' 'anObject' ).
]

{ #category : #tests }
SessionManagerRegistrationOrderTest >> testCreateCategoryFailsIfAfterUnexistentCategory [
	| customCategory |
	
	self 
		should: [customCategory := manager createCategory: 'Second' after: 'First']
		raise: NotFound.
]

{ #category : #tests }
SessionManagerRegistrationOrderTest >> testCustomCategoryGoesAfterSystemCategoryByDefault [
	| customCategory |
	customCategory := manager createCategory: 'Custom'.
	customCategory register: 'aCustomAction'.
	
	manager systemCategory register: 'aSystemAction' atPriority: 10.
	
	self 
		assert: manager startupList
		equals: #( 'aSystemAction' 'aCustomAction' ).
]

{ #category : #tests }
SessionManagerRegistrationOrderTest >> testCustomCategoryOrderIsPreserved [
	| customCategory |
	customCategory := manager createCategory: 'First'.
	customCategory register: 'first'.	
	
	customCategory := manager createCategory: 'Second' after: 'First'.
	customCategory register: 'second'.
	
	self 
		assert: manager startupList 
		equals: #( 'first' 'second' ).
]

{ #category : #tests }
SessionManagerRegistrationOrderTest >> testPriorityOrderInTheSameCategoryIsRespected [
	manager
		registerSystemClassNamed: 'aFirstSubscription' atPriority: 100;
		registerSystemClassNamed: 'aSecondSubscription' atPriority: 110;
		registerSystemClassNamed: 'aThirdSubscriptionThatShouldGoFirst' atPriority: 90.
		

	self 
		assertHandledId: manager startupList 
		equals: #( 'aThirdSubscriptionThatShouldGoFirst' 'aFirstSubscription' 'aSecondSubscription' ).
]

{ #category : #tests }
SessionManagerRegistrationOrderTest >> testRegisterElementByDefault [
	manager register: (ClassSessionHandler forClassNamed: 'anObject').

	self 
		assertHandledId: manager startupList
		equals: #( 'anObject' ).
]

{ #category : #tests }
SessionManagerRegistrationOrderTest >> testRegisterSystemElement [
	manager registerSystemClassNamed: 'anObject' atPriority: 1.

	self 
		assertHandledId: manager startupList
		equals: #( 'anObject' ).
]

{ #category : #tests }
SessionManagerRegistrationOrderTest >> testSubscriptionReverseOrder [
	manager
		registerSystemClassNamed: 'aFirstSubscription' atPriority: 100;
		register: (ClassSessionHandler forClassNamed: 'anObject').

	self 
		assertHandledId: manager startupList 
		equals: #( 'aFirstSubscription' 'anObject' ).
	self 
		assertHandledId: manager shutdownList 
		equals: #( 'anObject' 'aFirstSubscription' ).
]
