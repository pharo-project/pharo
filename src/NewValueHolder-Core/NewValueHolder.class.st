"
I am a class holding any object inside its unique instance variable. 
Each time the instance variable value changes, an announcement is emitted. 

The instance variable is accessed through `value` and `value:` while the registration is done by `whenChangedDo: aBlock`. 

In addition, infinite loops of propagation are prevented. 
Use case: you have two lists A, and B, and you want to keep their selection synchronised. 
So when A selection changes, you set B selection. 
But since B selection changes, you set A selection, and so on… 

This case is prevented by the use of a `lock` variable.
"
Class {
	#name : #NewValueHolder,
	#superclass : #Model,
	#instVars : [
		'lock',
		'value'
	],
	#category : #'NewValueHolder-Core-Base'
}

{ #category : #'instance creation' }
NewValueHolder class >> value: contents [

	^ self new
		rawValue: contents;
		yourself
]

{ #category : #converting }
NewValueHolder >> asValueHolder [

	^ self
]

{ #category : #backward }
NewValueHolder >> contents [

	^ self value
]

{ #category : #backward }
NewValueHolder >> contents: anObject [

	self value: anObject
]

{ #category : #private }
NewValueHolder >> handleCircularReferencesWhile: aBlock [
	"Used to handle circular references as explained in the class comment"

	lock ifTrue: [ ^ self ].
	lock := true.
	
	aBlock ensure: [ lock := false ].
]

{ #category : #initialization }
NewValueHolder >> initialize [

	super initialize.
	
	lock := false.
]

{ #category : #testing }
NewValueHolder >> isValueHolder [

	^ true
]

{ #category : #printing }
NewValueHolder >> printOn: aStream [
	super printOn: aStream.
	
	aStream 
		nextPutAll: '[ '; 
		print: self value;
		nextPutAll: ' ]'
]

{ #category : #accessing }
NewValueHolder >> rawValue: aValue [

	value := aValue
]

{ #category : #accessing }
NewValueHolder >> value [

	^ value
]

{ #category : #accessing }
NewValueHolder >> value: anObject [
	
	self handleCircularReferencesWhile: [ | oldValue |
		oldValue := value.
		value := anObject.
		self valueChanged: oldValue ].
]

{ #category : #accessing }
NewValueHolder >> valueChanged [
	
	self announcer announce: (ValueChanged newValue: value)
]

{ #category : #accessing }
NewValueHolder >> valueChanged: oldValue [
	
	self announcer announce: (ValueChanged oldValue: oldValue newValue: value)
]

{ #category : #accessing }
NewValueHolder >> valueChanged: oldValue to: newValue [
	
	self announcer announce: (ValueChanged oldValue: oldValue newValue: newValue)
]
