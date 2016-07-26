accessing
popupAutomaticDelay
	^popupAutomaticDelay ifNil: [ popupAutomaticDelay := self defaultPopupDelay ] 