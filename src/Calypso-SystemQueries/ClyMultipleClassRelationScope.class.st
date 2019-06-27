"
I represent complex hierarchy scope which merges together multiple kind of class relationship.
Class relations are represented by subclasses of ClySingleClassRelationScope. I use their #classesRelatedTo:do: method to enumerate all classes related to my basis:

	ClyMultipleClassRelationScope>>classesRelatedTo: aClass do: aBlock
		relationScopeClasses do: [ :each | 
			each classesRelatedTo: aClass do: aBlock ]

There is big difference between me and possible composition scope of other class relation scopes. 
For example try look at SomeTrait from the composition scope of ClySuperclassScope and ClyTraitUserScope.
First one will return no classes because traits has no superclass.
And the last one will return all classes and traits which uses given SomeTrait.

But my instance will behaves differently. For every class it will analyze both relationships: superclass and trait user. So every trait user will provide superclasses for the scope:

	ClyMultipleClassRelationScope of: SomeTrait merging: { ClySuperclassScope. ClyTraitUserScope }

From this scope you will see all classes and traits which uses given SomeTrait. But in addition it will show all superclasses of those classes and traits.

So idea behind me is to expand class visibility by multiple relationships. Every class retrieved from the basis will be recursivelly analyzed using all given relationships. 

Internal Representation and Key Implementation Points.

    Instance Variables
	relationScopeClasses:		<Collection of<ClySingleClassRelationScope class>>
"
Class {
	#name : #ClyMultipleClassRelationScope,
	#superclass : #ClyClassHierarchyScope,
	#instVars : [
		'relationScopeClasses'
	],
	#category : #'Calypso-SystemQueries-Scopes'
}

{ #category : #'instance creation' }
ClyMultipleClassRelationScope class >> of: basisObjects in: aNavigationEnvironment merging: singleRelationScopeClasses [
	^(self ofAll: basisObjects in: aNavigationEnvironment)
		relationScopeClasses: singleRelationScopeClasses
]

{ #category : #'instance creation' }
ClyMultipleClassRelationScope class >> of: basisObjects merging: singleRelationScopeClasses [
	^ (self ofAll: basisObjects)
		relationScopeClasses: singleRelationScopeClasses
]

{ #category : #comparing }
ClyMultipleClassRelationScope >> = anObject [
	"Answer whether the receiver and anObject represent the same object."

	self == anObject ifTrue: [ ^ true ].
	super = anObject ifFalse: [ ^false ].
	
	^ relationScopeClasses = anObject relationScopeClasses
]

{ #category : #converting }
ClyMultipleClassRelationScope >> asInterestingClassScope [
	^self copy
		relationScopeClasses: (relationScopeClasses collect: [:each | each asInterestingClassScope])
]

{ #category : #queries }
ClyMultipleClassRelationScope >> classesRelatedTo: aClass do: aBlock [
	relationScopeClasses do: [ :each | 
		each classesRelatedTo: aClass do: aBlock ]
]

{ #category : #comparing }
ClyMultipleClassRelationScope >> hash [
	"Answer an integer value that is related to the identity of the receiver."

	^super hash bitXor: relationScopeClasses hash
]

{ #category : #accessing }
ClyMultipleClassRelationScope >> relationScopeClasses [
	^ relationScopeClasses
]

{ #category : #accessing }
ClyMultipleClassRelationScope >> relationScopeClasses: anObject [
	relationScopeClasses := anObject
]

{ #category : #testing }
ClyMultipleClassRelationScope >> representsScope: aScopeClass [
	^(super representsScope: aScopeClass)
		or: [ relationScopeClasses anySatisfy: [ :each | each includesBehavior: aScopeClass ] ]
]
