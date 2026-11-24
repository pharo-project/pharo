Extension { #name : 'ReCriticEngine' }

{ #category : '*Renraku-UI' }
ReCriticEngine class >> guidedBy: anEntity ban: aCritique [

	^ self uniqueInstance guidedBy: anEntity ban: aCritique
]

{ #category : '*Renraku-UI' }
ReCriticEngine >> guidedBy: anEntity ban: aCritique [

	| candidates presenter |
	candidates := anEntity banLevelCandidates.
	presenter := SpSelectDialog new.
	(presenter
		title: 'On which level do you want to ban the rule?';
		items: candidates;
		display: [ :each | each reBanName ];
		onAccept: [ :dialog |
			dialog presenter inform: dialog presenter selectedItem asString ];
		openModal ) ifNotNil: [
		:entity | entity ban: aCritique ]
]
