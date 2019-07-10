"
I represent class scope of all inherited traits excluding basis.
I see methods of all traits which are used by my basis classes or traits
"
Class {
	#name : #ClyInheritedTraitScope,
	#superclass : #ClySingleClassRelationScope,
	#category : #'Calypso-SystemPlugins-Traits-Queries'
}

{ #category : #accessing }
ClyInheritedTraitScope class >> classesRelatedTo: aClass do: aBlock [
	"Do nothing. Traits are NOT inherited."
]

{ #category : #'inherited scope' }
ClyInheritedTraitScope class >> createInheritedScopeFrom: aClassScope [
	^(aClassScope asScope: ClyMultipleClassRelationScope)
		relationScopeClasses: { ClySuperclassScope. self }
]

{ #category : #'inherited scope' }
ClyInheritedTraitScope class >> createInheritingScopeFrom: aClassScope [
	^(aClassScope asScope: ClyMultipleClassRelationScope)
		relationScopeClasses: { ClySubclassScope. ClyTraitUserScope }
]

{ #category : #accessing }
ClyInheritedTraitScope class >> defaultName [
	^'traits'
]

{ #category : #'inherited scope' }
ClyInheritedTraitScope class >> inheritedScopeProvider [
	<classAnnotation>
	
	^ClyInheritedScopeProvider new 
		priority: 100
]
