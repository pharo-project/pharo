Extension { #name : 'ReMethodSourceCleaner' }

{ #category : '*Renraku-UI' }
ReMethodSourceCleaner class >> showCleaningOf: aString [

	SpDiffPresenter new
 		showOptions: false;
		leftText: aString;
		rightText: (self clean: aString);
		open
]
