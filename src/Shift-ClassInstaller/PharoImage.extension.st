Extension { #name : 'PharoImage' }

{ #category : '*Shift-ClassInstaller' }
PharoImage >> anonymousClassInstaller [
	"Answer the class responsible of creating classes in the system."

	^ ShiftAnonymousClassInstaller
]

{ #category : '*Shift-ClassInstaller' }
PharoImage >> classBuilder [
	"Answer the object responsible of creating subclasses of myself in the system."

	^ self classInstaller new builder
]

{ #category : '*Shift-ClassInstaller' }
PharoImage >> classInstaller [
	"Answer the class responsible of creating classes in the system."

	^ ShiftClassInstaller
]
