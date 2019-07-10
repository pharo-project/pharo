"
I represent visibility of methods of concrete class.
My extra scope is created for single class: 
	visibleScope asScope: ClyClassScope of: aConcreteSuperclass
	
My instances are created using following method:

	ClySuperclassVisibilityLevel of: aClassScope toShow: aClass
"
Class {
	#name : #ClySuperclassVisibilityLevel,
	#superclass : #ClyMethodVisibilityLevel,
	#category : #'Calypso-SystemQueries-Domain'
}

{ #category : #'instance creation' }
ClySuperclassVisibilityLevel class >> of: aClassScope toShow: aClass [
	^(self of: aClassScope)
		extraClassScope: (aClassScope asScope: ClyClassScope of: aClass)
]
