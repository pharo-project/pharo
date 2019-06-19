"
I am a root of hierarchy of class group providers which build just one group for given package scope.

While class group is based on class query my subclasses should implement two methods: 

- createClassQueryFrom: aPackageScope
It should return class query representing classes for the group.

- createClassGroupFor: aClassQuery from: aPackageScope
It should creates group instance using given scope. The package scope is given to build subgroups query if needed. Any class group can be expanded to children subgroups
"
Class {
	#name : #ClySingleClassGroupProvider,
	#superclass : #ClyClassGroupProvider,
	#category : #'Calypso-SystemQueries-Domain'
}

{ #category : #'building groups' }
ClySingleClassGroupProvider >> classGroupsIn: aPackageScope do: aBlock [
	
	| classQuery |
	classQuery := self createClassQueryFrom: aPackageScope.
	classQuery hasEmptyResult ifTrue: [ ^self ].
	
	aBlock value: (self createClassGroupFor: classQuery from: aPackageScope)
]

{ #category : #'building groups' }
ClySingleClassGroupProvider >> createClassGroupFor: aClassQuery from: aPackageScope [
	self subclassResponsibility 
]

{ #category : #'building groups' }
ClySingleClassGroupProvider >> createClassQueryFrom: aPackageScope [
	self subclassResponsibility 
]
