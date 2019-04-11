Extension { #name : #Behavior }

{ #category : #'*UndefinedClasses' }
Behavior >> isDefined [
	
	^ self isUndefined not
]

{ #category : #'*UndefinedClasses' }
Behavior >> isUndefined [
	
	^ self instanceSide inheritsFrom: UndefinedClass
]
