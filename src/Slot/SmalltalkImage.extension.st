Extension { #name : #SmalltalkImage }

{ #category : #'*Slot' }
SmalltalkImage >> anonymousClassInstaller [
	"Answer the class responsible of creating classes in the system."
	
	^ AnonymousClassInstaller
]

{ #category : #'*Slot' }
SmalltalkImage >> classBuilder [
	"Answer the object responsible of creating subclasses of myself in the system."
	
	^ self classInstaller new builder
]

{ #category : #'*Slot' }
SmalltalkImage >> classInstaller [
	"Answer the class responsible of creating classes in the system."
	
	^ PharoClassInstaller
]
