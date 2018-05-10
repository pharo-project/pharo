"
I represent a global variable.
"
Class {
	#name : #NECGlobalEntry,
	#superclass : #NECEntry,
	#category : #'NECompletion-Model'
}

{ #category : #operations }
NECGlobalEntry >> guessTypeWith: anECContext [ 
	| globalEntry |
	globalEntry := Smalltalk at: contents ifAbsent: [^ nil].
	globalEntry isBehavior ifTrue: [^ globalEntry].
	globalEntry ifNotNil: [^ globalEntry class].
	^ nil
]

{ #category : #accessing }
NECGlobalEntry >> label [
	^ 'global'
]

{ #category : #accessing }
NECGlobalEntry >> type [
	^ #globalVar
]
