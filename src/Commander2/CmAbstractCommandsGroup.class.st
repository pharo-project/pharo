"
I am an abstract class defining the API a command group should understand.

See my subclasses for concrete examples.
"
Class {
	#name : #CmAbstractCommandsGroup,
	#superclass : #CmObject,
	#category : #'Commander2-Groups'
}

{ #category : #'public-api' }
CmAbstractCommandsGroup >> / aString [
	"Alias for #commandOrRegisterNamed:."
	^ self commandOrGroupNamed: aString
]

{ #category : #visiting }
CmAbstractCommandsGroup >> acceptVisitor: aCmCommandOrGroup [
	^ aCmCommandOrGroup visitCommandGroup: self
]

{ #category : #accessing }
CmAbstractCommandsGroup >> allCommands [
	^ self subclassResponsibility
]

{ #category : #'public-api' }
CmAbstractCommandsGroup >> commandOrGroupNamed: aString [
	^ self subclassResponsibility
]

{ #category : #'public-api' }
CmAbstractCommandsGroup >> commands [
	^ self subclassResponsibility
]

{ #category : #'public-api' }
CmAbstractCommandsGroup >> entries [
	^ self subclassResponsibility
]

{ #category : #'public-api' }
CmAbstractCommandsGroup >> groups [
	^ self subclassResponsibility
]

{ #category : #testing }
CmAbstractCommandsGroup >> hasCommand: aCmCommand [
	^ self subclassResponsibility
]

{ #category : #testing }
CmAbstractCommandsGroup >> hasEntryNamed: aString [
	^ self subclassResponsibility
]

{ #category : #testing }
CmAbstractCommandsGroup >> isGroup [
	^ true
]

{ #category : #'public-api' }
CmAbstractCommandsGroup >> register: aCmCommandsOrRegister [
	^ self subclassResponsibility
]

{ #category : #'public-api' }
CmAbstractCommandsGroup >> unregister: aCmCommandOrRegister [
	^ self subclassResponsibility
]
