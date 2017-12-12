Extension { #name : #SmalltalkImage }

{ #category : #'*Shift-ClassInstaller' }
SmalltalkImage >> anonymousClassInstaller [
	"Answer the class responsible of creating classes in the system."
	
	^ ShiftAnonymousClassInstaller
]

{ #category : #'*Shift-ClassInstaller' }
SmalltalkImage >> classBuilder [
	"Answer the object responsible of creating subclasses of myself in the system."
	
	^ self classInstaller new builder
]

{ #category : #'*Shift-ClassInstaller' }
SmalltalkImage >> classInstaller [
	"Answer the class responsible of creating classes in the system."
	
	^ ShiftClassInstaller
]
