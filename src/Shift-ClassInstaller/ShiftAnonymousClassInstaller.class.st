"
I can generate classes without registering in the environment.
I should not be accessed directly, but by the accessor in Smalltalk or in  the classes.

I have exactly the same interface than the main class installer.
"
Class {
	#name : #ShiftAnonymousClassInstaller,
	#superclass : #ShiftClassInstaller,
	#category : #'Shift-ClassInstaller-Base'
}

{ #category : #accessing }
ShiftAnonymousClassInstaller >> comment: newClass [

]

{ #category : #building }
ShiftAnonymousClassInstaller >> installInEnvironment: newClass [
	^ self
]

{ #category : #building }
ShiftAnonymousClassInstaller >> installSubclassInSuperclass: newClass [
]

{ #category : #building }
ShiftAnonymousClassInstaller >> lookupOldClass [
	^ self
]

{ #category : #notifications }
ShiftAnonymousClassInstaller >> notifyChanges [
	
]

{ #category : #notifications }
ShiftAnonymousClassInstaller >> recategorize: newClass to: category [

]
