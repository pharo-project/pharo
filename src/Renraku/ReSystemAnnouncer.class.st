"
I announnce events related to Renraku and it's users.

To see the possilbe announcements check ReCriticEvent hierarchy.

To subscribe use:

ReSystemAnnouncer uniqueInstance week ...
"
Class {
	#name : #ReSystemAnnouncer,
	#superclass : #Announcer,
	#traits : 'TSingleton',
	#classTraits : 'TSingleton classTrait',
	#category : #'Renraku-Announcements'
}

{ #category : #announce }
ReSystemAnnouncer >> notifyCritique: aCritique descriptionViewedFor: anEntity [

	self announce: (
		ReCritiqueDescriptionViewed new 
			critique: aCritique;
			entity: anEntity;
			yourself)
  
]

{ #category : #announce }
ReSystemAnnouncer >> notifyCritique: aCritique of: anEntity bannedOn: anEntityBanLevel [

	self announce: (
		ReCritiqueBanned new
			critique: aCritique;
			entity: anEntity;
			entityBanLevel: anEntityBanLevel;
			yourself)

]

{ #category : #announce }
ReSystemAnnouncer >> notifyEntity: anEntity criticizedWith: critiques [

	self announce: (
		ReEntityCriticized new 
			critiques: critiques;
			entity: anEntity;
			yourself)

]
