private
chooseDate

	| calendar defaultDate |
	defaultDate := self date ifNil: [ self defaultDate ].
	calendar := CalendarMorph openOn: defaultDate.
	calendar
		center: calendar cursorPoint;
		fitInWorld;
		onChoiceSend: #onDateChosen: to: self.