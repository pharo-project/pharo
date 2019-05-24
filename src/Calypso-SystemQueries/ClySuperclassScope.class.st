"
I represent scope of all superclasses of my basis classes.  
I show all superclasses, their methods and variables
"
Class {
	#name : #ClySuperclassScope,
	#superclass : #ClySingleClassRelationScope,
	#category : #'Calypso-SystemQueries-Scopes'
}

{ #category : #converting }
ClySuperclassScope class >> asInterestingClassScope [
	^ClyInterestingSuperclassScope 
]

{ #category : #accessing }
ClySuperclassScope class >> classesRelatedTo: aClass do: aBlock [
	aClass isRootInEnvironment ifTrue: [ ^self ].
	
	aBlock value: aClass superclass
]

{ #category : #'inherited scope' }
ClySuperclassScope class >> createInheritedScopeFrom: aClassScope [
	^aClassScope asScope: self
]

{ #category : #'inherited scope' }
ClySuperclassScope class >> createInheritingScopeFrom: aClassScope [
	^aClassScope asScope: ClySubclassScope
]

{ #category : #accessing }
ClySuperclassScope class >> defaultName [
	^'superclasses'
]

{ #category : #'inherited scope' }
ClySuperclassScope class >> inheritedScopeProvider [
	<classAnnotation>
	
	^ClyInheritedScopeProvider new
]

{ #category : #converting }
ClySuperclassScope >> asInterestingClassScope [
	^self asScope: ClyInterestingSuperclassScope 
]
