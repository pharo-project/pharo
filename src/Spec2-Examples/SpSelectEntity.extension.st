Extension { #name : #SpSelectEntity }

{ #category : #'*Spec2-Examples' }
SpSelectEntity class >> example [
	self new
		selectDialog: [ SpListPresenter new
				items: #(Henrik Peter);
				yourself ];
		displaySymbol: #asString;
		placeholder: 'Names';
		possibleEntities: [ #(Diego Stephan Norbert Stephane Marcus Torsten Sven Sean) sorted ];
		openDialogWithSpec
]
