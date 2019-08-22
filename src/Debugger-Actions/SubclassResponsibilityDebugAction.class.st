"
A SubclassResponsabilityDebugAction is a debugging action that can create a method if the debugger was opened as a result of a #subclassResponsibility mesage send.


"
Class {
	#name : #SubclassResponsibilityDebugAction,
	#superclass : #DebugAction,
	#category : #'Debugger-Actions-Actions'
}

{ #category : #registration }
SubclassResponsibilityDebugAction class >> actionType [
	<debuggingAction>
]

{ #category : #testing }
SubclassResponsibilityDebugAction >> appliesToDebugger: aDebugger [
	| interruptedContext |
	
	interruptedContext := aDebugger session interruptedContext.
	^ (interruptedContext selector == #subclassResponsibility) and: [ 
		"If we work with an instance of the abstract class (shouldn't normally happen) do not add this action, 
		as the abstract class alredy contains the the method."
		interruptedContext sender receiver class ~~ interruptedContext sender methodClass  ]


]

{ #category : #private }
SubclassResponsibilityDebugAction >> askForSuperclassOf: aClass to: aSuperclass toImplement: aSelector ifCancel: cancelBlock [
	| classes currentSuperclass chosenClassIndex |
	
	classes := OrderedCollection new.
	classes add: aClass.
	currentSuperclass := aClass superclass.
	[ currentSuperclass == nil or: [ currentSuperclass == aSuperclass ] ]
		whileFalse: [
			classes add: currentSuperclass.
			currentSuperclass := currentSuperclass superclass ].
	classes := classes addAll: (aClass traits sort: [ :t1 :t2 | t1 asString < t2 asString ]); yourself.
	
	classes size = 1 ifTrue: [ ^ classes at: 1 ].
	chosenClassIndex := UIManager default 
		chooseFrom: (classes collect: [:c | c name])
		title: 'Define #', aSelector, ' in which class?'.
	chosenClassIndex = 0 ifTrue: [^ cancelBlock value].
	^ classes at: chosenClassIndex
]

{ #category : #accessing }
SubclassResponsibilityDebugAction >> defaultLabel [

	^  'Create'
]

{ #category : #accessing }
SubclassResponsibilityDebugAction >> defaultOrder [

	^ 1
]

{ #category : #actions }
SubclassResponsibilityDebugAction >> executeAction [
	| senderContext msg msgCategory chosenClass |
	
	senderContext := self interruptedContext sender.
	msg := Message 
		selector: senderContext selector
		arguments: senderContext arguments.
	msgCategory := senderContext methodClass organization categoryOfElement: msg selector.
	
	chosenClass := self 
		askForSuperclassOf: senderContext receiver class 
		to: senderContext methodClass 
		toImplement: senderContext selector 
		ifCancel: [ ^ self ].

	self session 
		implement: msg 
		classified: msgCategory 
		inClass: chosenClass 
		forContext: senderContext.
]

{ #category : #accessing }
SubclassResponsibilityDebugAction >> help [
	^ 'Create a method in a class having the responsibility to implement it.'
]

{ #category : #accessing }
SubclassResponsibilityDebugAction >> id [

	^ #subclassResponsibility
]
