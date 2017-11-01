Extension { #name : #ReSystemAnnouncer }

{ #category : #'*qualityAssistant-announce' }
ReSystemAnnouncer >> notifyBanInitiatedFor: aQACritique of: anEntity [

	self announce: (
		QAInitiateCritiqueBan new
			critique: aQACritique;
			entity: anEntity;
			yourself)
	
]

{ #category : #'*qualityAssistant-announce' }
ReSystemAnnouncer >> notifyCritique: aCritique AutoFixedFor: anEntity [

	self announce: (
		ReCritiqueAutofixed new
			critique: aCritique;
			entity: anEntity;
			yourself)
]

{ #category : #'*qualityAssistant-announce' }
ReSystemAnnouncer >> notifyCritique: aCritique clickedOnFor: anEntity [ 
	
	self announce: (
		QACritiqueClickedOn new 
			critique: aCritique;
			entity: anEntity;
			yourself)
]

{ #category : #'*qualityAssistant-announce' }
ReSystemAnnouncer >> notifyCritiquesSeen: critiques of: anEntity [

	self announce: (
		QACritiquesSeen new
			critiques: critiques;
			entity: anEntity;
			yourself)
	
]

{ #category : #'*qualityAssistant-announce' }
ReSystemAnnouncer >> notifyViewedDiffFor: aQACritique of: anEntity [

	self announce: (
		QAViewedDiff new
			critique: aQACritique;
			entity: anEntity;
			yourself)
	
]
