"
I am a query of all method groups from given scope.

Scope should support #methodGroupsDo:. 
Currently it is only ClyAbstractClassScope.

The scope delegates method group building to the environment plugins.
It asks each plugin to collect method group providers using: 

	plugin collectMethodGroupProviders 

And then each provider creates set of method groups using method: 

	groupProvider buildGroupsFrom: aClassScope

The scope ensures that all methods from given scope will be enumerated only once.
So provider should return groups without looking into the methods of scope.
Then the scope itself will check that every built group includes at least one method:

	aGroup dependsOnMethod: aMethod

So scope will enumerate all available methods once and verify each group.

Scope also allow static groups which do not need any method check. Provider of such groups should return true from method #isStatic: 
	
	methodGroupProvider isStatic >>> true

Look at ClyMethodGroupProvider for more details
"
Class {
	#name : #ClyAllMethodGroups,
	#superclass : #ClyTypedQuery,
	#category : #'Calypso-SystemQueries-Queries'
}

{ #category : #'items type' }
ClyAllMethodGroups class >> resultItemsType [
	^ClyMethodGroup
]

{ #category : #execution }
ClyAllMethodGroups >> buildResult: aQueryResult [ 

	| groups |
	groups := OrderedCollection new.
	
	scope methodGroupsDo: [:group | groups add: group].
	
	aQueryResult fillWith: groups
]

{ #category : #execution }
ClyAllMethodGroups >> checkEmptyResult [
	
	scope methodGroupsDo: [ :each | ^false].
	
	^true
]

{ #category : #execution }
ClyAllMethodGroups >> collectMetadataOf: aQueryResult by: anEnvironmentPlugin [
	anEnvironmentPlugin collectMetadataOfMethodGroups: aQueryResult 
]

{ #category : #printing }
ClyAllMethodGroups >> description [

	^'all method groups'
]

{ #category : #'system changes' }
ClyAllMethodGroups >> isResult: aQueryResult affectedBy: aSystemAnnouncement [
	
	| asyncGroups |
	(scope includesMethodGroupsAffectedBy: aSystemAnnouncement) ifTrue: [ ^true ].
	
	(aSystemAnnouncement isKindOf: ClyAsyncQueryIsDone) ifTrue: [ 
		asyncGroups := aQueryResult items select: [ :each | each isKindOf: ClyAsyncMethodGroup ].
		(asyncGroups anySatisfy: [ :each | each asyncQueryResult == aSystemAnnouncement queryResult])
			ifTrue: [ ^true ]
	 ].

	^false
]

{ #category : #accessing }
ClyAllMethodGroups >> shortName [
	^'Methods'
]

{ #category : #converting }
ClyAllMethodGroups >> withMetaLevelScope: aScopeClass [ 
	^self withScope: (scope withMetaLevel: aScopeClass)
]
