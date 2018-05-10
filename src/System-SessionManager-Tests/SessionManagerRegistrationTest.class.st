"
SUnit tests for registration on the Pharo session manager
"
Class {
	#name : #SessionManagerRegistrationTest,
	#superclass : #TestCase,
	#instVars : [
		'manager'
	],
	#category : #'System-SessionManager-Tests'
}

{ #category : #running }
SessionManagerRegistrationTest >> setUp [
	super setUp.
	manager := SessionManager new
]

{ #category : #tests }
SessionManagerRegistrationTest >> testGivenARegisteredClassHandlerShouldHasRegisteredReturnsTrue [
	manager registerSystemClassNamed: #Foo.

	self assert: (manager hasRegistered: #Foo)
]

{ #category : #tests }
SessionManagerRegistrationTest >> testGivenARegisteredClassRenamedThenItsHandlerIsUpdated [
	manager registerSystemClassNamed: #Foo.
	manager renamedClass: self from: #Foo to: #Bar.
	
	self 
		assert: (manager startupList collect: #handledId)
		equals: #( Bar ).
]

{ #category : #tests }
SessionManagerRegistrationTest >> testGivenManyRegistrationsInDifferentCategoriesShoudlLastClassRegistrationBeTheOnlyOccurenceInStartupList [
	manager registerSystemClassNamed: #Foo.
	manager registerSystemClassNamed: #Bar.
	manager registerGuiClassNamed: #Foo atPriority: 30.
	manager registerGuiClassNamed: #Foo atPriority: 50.
	
	self 
		assert: (manager startupList collect: #handledId)
		equals: #( Bar Foo ).
]

{ #category : #tests }
SessionManagerRegistrationTest >> testGivenManyRegistrationsInSameCategoryAtDifferentPriorityShoudlAClassBePresentOnlyOnceInStartupList [
	manager registerSystemClassNamed: #Foo.
	manager registerSystemClassNamed: #Bar atPriority: 15.
	manager registerSystemClassNamed: #Foo atPriority: 10.
	manager registerSystemClassNamed: #Foo atPriority: 20.
	
	self 
		assert: (manager startupList collect: #handledId)
		equals: #( Bar Foo ).
]

{ #category : #tests }
SessionManagerRegistrationTest >> testGivenManyRegistrationsInSameCategoryShoudlAClassBePresentOnlyOnceInStartupList [
	manager registerSystemClassNamed: #Foo.
	manager registerSystemClassNamed: #Bar.
	manager registerSystemClassNamed: #Foo.
	manager registerSystemClassNamed: #Foo.
	
	self 
		assert: (manager startupList collect: #handledId)
		equals: #( Bar Foo ).
]

{ #category : #tests }
SessionManagerRegistrationTest >> testGivenUnknonwnHandledIdShouldHasRegisteredReturnsFalse [
	self deny: (manager hasRegistered: #Foo)
]
