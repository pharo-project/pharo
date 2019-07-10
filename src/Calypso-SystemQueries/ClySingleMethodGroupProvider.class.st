"
I am a root of hierarchy of simple method group providers which always supply single group for given class scope.
I simplify implementation of simple plugins which only want to define one extra method group. They should implement two methods: 

- createMethodQueryFrom: aClassScope
It should return method query representing methods for the group.

- createMethodGroupFor: aMethodQuery from: aClassScope
It should create group instance using given scope. The class scope is given to build subgroups query if needed. Any method group can be expanded to children subgroups

All my subclasses are not static by default. So they particupate in analysis of all methods in the scope
"
Class {
	#name : #ClySingleMethodGroupProvider,
	#superclass : #ClyMethodGroupProvider,
	#category : #'Calypso-SystemQueries-Domain'
}

{ #category : #'building groups' }
ClySingleMethodGroupProvider >> buildGroupsFrom: aClassScope [

	| methodQuery methodGroup |
	methodQuery := self createMethodQueryFrom: aClassScope.
	(self shouldBuildGroupWith: methodQuery) ifFalse: [ ^#() ].
		
	methodGroup := self createMethodGroupFor: methodQuery from: aClassScope.
	methodQuery isAsync ifTrue: [ methodGroup := methodGroup asAsyncQueryGroup ].
	
	^Array with: methodGroup
]

{ #category : #'building groups' }
ClySingleMethodGroupProvider >> createMethodGroupFor: aMethodQuery from: aClassScope [
	
	self subclassResponsibility 
]

{ #category : #'building groups' }
ClySingleMethodGroupProvider >> createMethodQueryFrom: aClassScope [
	self subclassResponsibility 
]

{ #category : #testing }
ClySingleMethodGroupProvider >> isStatic [
	^false
]

{ #category : #'system changes' }
ClySingleMethodGroupProvider >> providesGroupsAffectedBy: aSystemAnnouncement inScope: aClassScope [

	| query |
	query := self createMethodQueryFrom: aClassScope.
	^query isResult: ClyUnknownQueryResult instance affectedBy: aSystemAnnouncement
]

{ #category : #testing }
ClySingleMethodGroupProvider >> shouldBuildGroupWith: aMethodQuery [
	
	self isStatic ifFalse: [ ^true ].
	^aMethodQuery hasEmptyResult not
]
