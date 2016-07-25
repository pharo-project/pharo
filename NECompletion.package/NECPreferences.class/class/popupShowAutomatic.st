accessing
popupShowAutomatic
	^ popupShowAutomatic ifNil: [ popupShowAutomatic := self defaultPopupShowAutomatic ]