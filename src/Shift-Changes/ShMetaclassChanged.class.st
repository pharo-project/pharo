"
When there is a change in the metaclass structure I announce that change.
I announce with a copy of the old class and the new class.
"
Class {
	#name : #ShMetaclassChanged,
	#superclass : #ShAbstractChange,
	#category : #'Shift-Changes'
}

{ #category : #announcing }
ShMetaclassChanged >> announceChanges [
	SystemAnnouncer uniqueInstance classDefinitionChangedFrom: oldClass class to: builder newMetaclass.
	SystemAnnouncer uniqueInstance classModificationAppliedTo: builder newMetaclass
]

{ #category : #accessing }
ShMetaclassChanged >> builder: anObject [
	super builder: anObject.
	oldClass := builder oldClass copy.
]

{ #category : #testing }
ShMetaclassChanged >> hasToMigrateInstances [
	^ true
]

{ #category : #migration }
ShMetaclassChanged >> migrateInstancesOf: anOldClass to: newClass using: aClassInstaller [

	aClassInstaller migrateInstances: { anOldClass } to: newClass classSide
]

{ #category : #propagating }
ShMetaclassChanged >> propagateToSubclasses: anotherBuilder [
	anotherBuilder changes add: self.
]
