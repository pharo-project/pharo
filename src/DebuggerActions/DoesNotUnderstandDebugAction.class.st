"
A DoesNotUnderstandDebugAction is a debugging action that can create a method if the debugger was opened as a result of a #doesNotUnderstand mesage send.

"
Class {
	#name : #DoesNotUnderstandDebugAction,
	#superclass : #DebugAction,
	#category : #'Debugger-Actions-Actions'
}

{ #category : #registration }
DoesNotUnderstandDebugAction class >> actionType [
	<debuggingAction>
]

{ #category : #testing }
DoesNotUnderstandDebugAction >> appliesToDebugger: aDebugger [

	^ aDebugger session isInterruptedContextDoesNotUnderstand 
]

{ #category : #private }
DoesNotUnderstandDebugAction >> askForCategoryIn: aClass default: aString [
	| categoryName |
	categoryName := AbstractTool requestProtocolNameFor: aClass initialAnswer: aString.
	categoryName ifNil: [^aString].
	^ categoryName isEmptyOrNil 
		ifTrue: [^ aString] 
		ifFalse: [ categoryName ]
]

{ #category : #private }
DoesNotUnderstandDebugAction >> askForSuperclassOf: aClass toImplement: aSelector ifCancel: cancelBlock [
	| classes chosenClassIndex |
	classes := aClass withAllSuperclasses addAll: (aClass traits sort: [ :t1 :t2 | t1 asString < t2 asString ]); yourself.
	chosenClassIndex := UIManager default 
		chooseFrom: (classes collect: [:c | c name])
		title: 'Define #', aSelector, ' in which class?'.
	chosenClassIndex = 0 ifTrue: [^ cancelBlock value].
	^ classes at: chosenClassIndex
]

{ #category : #private }
DoesNotUnderstandDebugAction >> closeWindow [
	"Safely close the debugger window without destroying the debugger context and stack"
	| modelWindow |
	modelWindow := self debugger window.
	modelWindow
		model: nil;
		close
]

{ #category : #private }
DoesNotUnderstandDebugAction >> createMissingClassWith: variableNode in: aContext [
	|  previousContext  |
		
	OCUndeclaredVariableWarning new
		node: variableNode;
		defineClass: variableNode name.
		
	previousContext := aContext sender.
	
	self closeWindow.
	
	previousContext restart
]

{ #category : #accessing }
DoesNotUnderstandDebugAction >> defaultLabel [

	^  'Create'
]

{ #category : #accessing }
DoesNotUnderstandDebugAction >> defaultOrder [

	^ 1
]

{ #category : #actions }
DoesNotUnderstandDebugAction >> executeAction [
	"Should only be called when the debugger was created in response to a
	MessageNotUnderstood exception. Create a stub for the method that was
	missing and proceed into it, or create a class if it was missing instead"
	
	| msg msgCategory chosenClass exception |
	
	msg := self interruptedContext tempAt: 1.
	exception := self interruptedContext tempAt: 2.
	
	(exception class == VariableNotDeclared) ifTrue: [ 
		self createMissingClassWith: exception variableNode in: self interruptedContext ].
	
	chosenClass := self 
		askForSuperclassOf: self interruptedContext receiver class
		toImplement: msg selector
		ifCancel: [^self].
	msgCategory := (self askForCategoryIn: chosenClass default: 'as yet unclassified').
	self  session
		implement: msg 
		classified: msgCategory 
		inClass: chosenClass 
		forContext: self interruptedContext.
	self debugger selectTopContext
]

{ #category : #accessing }
DoesNotUnderstandDebugAction >> help [
    ^ 'Create the missing class or method in the user prompted class, and restart the debugger at the location where it can be edited.'
]

{ #category : #accessing }
DoesNotUnderstandDebugAction >> id [

	^ #doesNotUnderstand
]
