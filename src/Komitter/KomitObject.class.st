"
I'm the superclass keeping track of the state of the object (added/modified/removed) but according the kind of object the changes apply to. My subclasses KomitClass, KomitDefinition, KomitPackage and KomitMethod wrap MCAddition/MCRemoval/MCDefinition. 

While the hierarchy of MCDefinition is about the kind of change (addition, removal, definition),
the current hierarchy is about the kind of objects changes apply to.

Now it would be interested to see if this hierarchy has been created because of lack of awareness that the hierarchy of MCDefinition is already proposing such facilities. 

"
Class {
	#name : #KomitObject,
	#superclass : #Object,
	#instVars : [
		'added',
		'modified',
		'removed',
		'selected',
		'expanded'
	],
	#category : 'Komitter-Models'
}

{ #category : #comparing }
KomitObject >> <= aKomitObject [ 

	^ self subclassResponsibility
]

{ #category : #accessing }
KomitObject >> added [
	^ added
]

{ #category : #accessing }
KomitObject >> added: anObject [
	added := anObject
]

{ #category : #accessing }
KomitObject >> expanded [
	^ expanded
]

{ #category : #accessing }
KomitObject >> expanded: anObject [
	expanded := anObject
]

{ #category : #protocol }
KomitObject >> flush [
	
	added := false.
	modified := false.
	removed := false.
]

{ #category : #initialize }
KomitObject >> initialize [

	super initialize.

	added := false.
	modified := false.
	removed := false.
	
	selected := true.
	expanded := false
]

{ #category : #testing }
KomitObject >> isCommitable [

	^ false
]

{ #category : #testing }
KomitObject >> isDefinition [

	^ false
]

{ #category : #testing }
KomitObject >> isDirty [

	^ self added or: [ self modified or: [ self removed ] ]
]

{ #category : #testing }
KomitObject >> isKomitClass [

	^ false
]

{ #category : #testing }
KomitObject >> isMethod [

	^ false
]

{ #category : #testing }
KomitObject >> isPackage [

	^ false
]

{ #category : #accessing }
KomitObject >> modified [
	^ modified
]

{ #category : #accessing }
KomitObject >> modified: anObject [
	modified := anObject
]

{ #category : #accessing }
KomitObject >> removed [
	^ removed
]

{ #category : #accessing }
KomitObject >> removed: anObject [
	removed := anObject
]

{ #category : #accessing }
KomitObject >> selected [
	^ selected
]

{ #category : #accessing }
KomitObject >> selected: anObject [
	selected := anObject
]
