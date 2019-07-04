"
I am abstract query of all classes from given scope.
Subclasses should define concrete condition which all retrieved classes should satisfy: 
- selectsClass: aClass 

Scope should support #classesDo: which I use to collect and filter all available classes.

I provide several convenient methods to instantiate queries with hierarchical result:

	ClyAllClasses hierarchical
	ClyAllClasses hierarchicalFrom: aScope
	
In that cases instances will be created with ClyHierarchicallySortedClasses required result
"
Class {
	#name : #ClyClassQuery,
	#superclass : #ClyTypedQuery,
	#category : #'Calypso-SystemQueries-Queries'
}

{ #category : #'instance creation' }
ClyClassQuery class >> hierarchical [

	^self as: ClyHierarchicallySortedClasses new
]

{ #category : #'instance creation' }
ClyClassQuery class >> hierarchicalFrom: aScope [

	^self from: aScope as: ClyHierarchicallySortedClasses new
]

{ #category : #'items type' }
ClyClassQuery class >> resultItemsType [
	^ClyClass
]

{ #category : #execution }
ClyClassQuery >> buildResult: aQueryResult [
	| filteredClasses |
	filteredClasses := Set new.
	
	scope classesDo: [ :each | 
		(self selectsClass: each) ifTrue: [filteredClasses add: each]].
	
	aQueryResult fillWith: filteredClasses
]

{ #category : #execution }
ClyClassQuery >> checkEmptyResult [

	scope classesDo: [ :each | 
		(self selectsClass: each) ifTrue: [^false]].
	
	^true
]

{ #category : #execution }
ClyClassQuery >> collectMetadataOf: aQueryResult by: anEnvironmentPlugin [
	anEnvironmentPlugin collectMetadataOfClasses: aQueryResult 
]

{ #category : #'system changes' }
ClyClassQuery >> isResult: aQueryResult affectedBy: aSystemAnnouncement [
	
	(scope includesClassesAffectedBy: aSystemAnnouncement) ifTrue: [ ^true ].
	
	(scope canDetectAffectOnClassesBy: aSystemAnnouncement) ifTrue: [ ^false ].	
	"In some scopes we should check every class that it was affected by event
	For example class hierarchy scope could not track that removed class was in hierarchy using only events. 
	It should be improved in system but right now it is like that"					
	aQueryResult rawItemsDo: [ :each |
		(aSystemAnnouncement affectsClass: each) ifTrue: [^true]].	
	^false
]

{ #category : #testing }
ClyClassQuery >> retrievesItem: aClass [
	^self selectsClass: aClass
]

{ #category : #testing }
ClyClassQuery >> selectsClass: aClass [
	self subclassResponsibility 
]
