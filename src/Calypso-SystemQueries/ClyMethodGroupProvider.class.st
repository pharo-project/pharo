"
I am a root of hierarchy of method group providers.
My subclasses build method groups for given class scope. They should implement: 

- buildGroupsFrom: aClassScope

Implementors should not think about it emptiness of created groups. Groups query filters reduntant groups by itself.
But subclasses can specify that group should be always present using method #isStatic. By default any provider defines static groups. 
Static groups are not depends the count of methods which they provide
"
Class {
	#name : #ClyMethodGroupProvider,
	#superclass : #Object,
	#category : #'Calypso-SystemQueries-Domain'
}

{ #category : #'building groups' }
ClyMethodGroupProvider >> buildGroupsFrom: aClassScope [ 
	self subclassResponsibility 
]

{ #category : #testing }
ClyMethodGroupProvider >> isStatic [
	^true
]

{ #category : #'system changes' }
ClyMethodGroupProvider >> providesGroupsAffectedBy: aSystemAnnouncement inScope: aClassScope [

	aClassScope classesDo: [ :each | 
		(aSystemAnnouncement affectsClass: each) ifTrue: [ ^true ]].
		
	^false
]
