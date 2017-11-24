"
This exception is raised when mutating a read-only object.

My instances have 4 fields to be able to reproduce the modification through retryModification method.

object <Object> read-only object that the code attempted to mutate
index <SmallInteger> index of the field in the object mutated, relevant for the corresponding selector
value <Object> value that was attempted to be stored into the read-only object
selector <Symbol> selector that can be used to reproduce the mutation (typically, #at:put:, #instVarAt:put:, etc.)
"
Class {
	#name : #ModificationForbidden,
	#superclass : #Error,
	#instVars : [
		'object',
		'fieldIndex',
		'newValue',
		'retrySelector'
	],
	#category : #'Kernel-Exceptions'
}

{ #category : #'instance creation' }
ModificationForbidden class >> for: anObject atIndex: fieldIndex with: newValue [

	^self new 
		object: anObject;
		fieldIndex: fieldIndex;
		newValue: newValue;
		retrySelector: #at:put:
]

{ #category : #'instance creation' }
ModificationForbidden class >> for: anObject atInstVar: fieldIndex with: newValue [

	^self new 
		object: anObject;
		fieldIndex: fieldIndex;
		newValue: newValue;
		retrySelector: #instVarAt:put:
]

{ #category : #accessing }
ModificationForbidden >> fieldIndex [
	^ fieldIndex
]

{ #category : #accessing }
ModificationForbidden >> fieldIndex: anObject [
	fieldIndex := anObject
]

{ #category : #testing }
ModificationForbidden >> isResumable [
	"Determine whether an exception is resumable."

	^true
]

{ #category : #printing }
ModificationForbidden >> messageText [
	"Overwritten to initialiaze the message text to a standard text if it has not yet been set"
	
	^ messageText ifNil: [ messageText := self standardMessageText ]
]

{ #category : #accessing }
ModificationForbidden >> newValue [
	^ newValue
]

{ #category : #accessing }
ModificationForbidden >> newValue: anObject [
	newValue := anObject
]

{ #category : #accessing }
ModificationForbidden >> object [
	^ object
]

{ #category : #accessing }
ModificationForbidden >> object: anObject [
	object := anObject
]

{ #category : #printing }
ModificationForbidden >> printObject: obj on: s [
	[obj printOn: s] on: Exception do: [ :ex | s << '<cannot print object>' ]
]

{ #category : #retrying }
ModificationForbidden >> retryModification [
	object perform: retrySelector with: fieldIndex with: newValue.
	self resume: newValue
]

{ #category : #accessing }
ModificationForbidden >> retrySelector [
	^ retrySelector
]

{ #category : #accessing }
ModificationForbidden >> retrySelector: anObject [
	retrySelector := anObject
]

{ #category : #printing }
ModificationForbidden >> standardMessageText [
	^ String streamContents: [ :s |
		s << ' '.
		self printObject: object on: s. 
		s << ' is read-only, hence its field '.
		fieldIndex printOn: s.
		s << ' cannot be modified with '.
		self printObject: newValue on: s]
]
