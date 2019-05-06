Extension { #name : #SelectEntity }

{ #category : #'*Spec-Examples' }
SelectEntity class >> example [
	self new
		selectDialog: [ ListPresenter new
				items: #(Henrik Peter);
				yourself ];
		displaySymbol: #asString;
		placeholder: 'Names';
		possibleEntities: [ #(Diego Stephan Norbert Stephane Marcus Torsten Sven Sean) sorted ];
		openDialogWithSpec
]
