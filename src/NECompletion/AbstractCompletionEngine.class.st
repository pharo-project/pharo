"
I am an abstract class defining the main interface of a completion engine.

- A completion engine is subscribed to a widget and receives events to provide completion.
- Then, when the completion engine decides to accept an element of the completion, the completion engine knows how to modify the underlying text widget.

I'm also used by the NECPreferences to detect the available engines in the system by checking my subclasses. It is important that I'm here, so there are no circular dependencies between the Morphic specific Completion engine and the abstract backend.
"
Class {
	#name : #AbstractCompletionEngine,
	#superclass : #Object,
	#category : #'NECompletion-Model'
}

{ #category : #testing }
AbstractCompletionEngine class >> isAbstract [

	^ self == AbstractCompletionEngine
]

{ #category : #testing }
AbstractCompletionEngine >> hasCompletionContext [

	self subclassResponsibility
]
