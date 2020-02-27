"
I am an abstract class defining the API a command should understand.

See my subclasses for concrete examples.
"
Class {
	#name : #CmAbstractCommand,
	#superclass : #CmObject,
	#category : #'Commander2-Commands'
}

{ #category : #'instance creation' }
CmAbstractCommand class >> forContext: anObject [
	"Creates a new command with anObject as context."
	^ self new
		context: anObject;
		yourself
]

{ #category : #visiting }
CmAbstractCommand >> acceptVisitor: aCmCommandOrGroup [
	^ aCmCommandOrGroup visitCommand: self
]

{ #category : #testing }
CmAbstractCommand >> canBeExecuted [
	"This hook allows the command decide if it can be run with the context it holds or not.
	 It should return a boolean.
	"
	^ self subclassResponsibility
]

{ #category : #accessing }
CmAbstractCommand >> context [
	^ self subclassResponsibility
]

{ #category : #accessing }
CmAbstractCommand >> context: anObject [
	^ self subclassResponsibility
]

{ #category : #executing }
CmAbstractCommand >> execute [
	"Execute the actions that should be done by the command.
	 This method expect that the context has been put in #context inst. var. if any context is relevant."
	self subclassResponsibility
]

{ #category : #testing }
CmAbstractCommand >> isCommand [
	^ true
]
