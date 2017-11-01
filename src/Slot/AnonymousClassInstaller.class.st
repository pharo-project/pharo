"
I am a class installer that does not expose the class to the global environment nor announces that a new class has been created.
As it's name suggest it is used for anonymous classes, for instance in Tests.
"
Class {
	#name : #AnonymousClassInstaller,
	#superclass : #AbstractClassInstaller,
	#category : 'Slot-ClassBuilder'
}

{ #category : #notifications }
AnonymousClassInstaller >> classAdded: aClass inCategory: aCategory [
]

{ #category : #accessing }
AnonymousClassInstaller >> classAt: aName ifAbsent: aBlock [
	^ aBlock value
]

{ #category : #notifications }
AnonymousClassInstaller >> classDefinitionDeeplyChangedFrom: oldClass to: newClass by: classModification [
]

{ #category : #notifications }
AnonymousClassInstaller >> classDefinitionShallowChangedFrom: oldClass to: newClass by: classModification [
]

{ #category : #accessing }
AnonymousClassInstaller >> environment: anEnvironment [
]

{ #category : #migrating }
AnonymousClassInstaller >> migrateClasses: old to: new using: anInstanceModification [
]

{ #category : #notifications }
AnonymousClassInstaller >> recategorize: aClass to: aCategory [
]
