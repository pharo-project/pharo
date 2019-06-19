"
I am a root of critique queries hierarchy.
My subclasses are supposed to return critiques instances.

I implement system changes processing and metadata collection logic.
"
Class {
	#name : #ClyCritiqueQuery,
	#superclass : #ClyTypedQuery,
	#category : #'Calypso-SystemPlugins-Critic-Queries'
}

{ #category : #'items type' }
ClyCritiqueQuery class >> resultItemsType [
	^ReAbstractCritique
]

{ #category : #execution }
ClyCritiqueQuery >> collectMetadataOf: aQueryResult by: anEnvironmentPlugin [
	anEnvironmentPlugin resolveMetadataOfCritiques: aQueryResult 
]

{ #category : #'system changes' }
ClyCritiqueQuery >> isResult: aQueryResult affectedBy: aSystemAnnouncement [
	"We can not compute affect on critiques by any kind system change
	because critiques are composed by some external system
	So without real knowledge we can only update critiqies after any system change.
	It can of course lead to very poor system performance because critic computation is slow process
	and triggering it after every system change can slow down system a lot.
	So instead we just reset given aQueryResult silently. So users should update views manually"
	aSystemAnnouncement affectsCritiques ifFalse: [ ^false ].
	
	aQueryResult forceLazyRebuild.	
	^false "and we return false to prevent result observers to be notified"
]
