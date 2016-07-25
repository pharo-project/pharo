private
chooseDate

	| calendar |
	calendar := CalendarMorph openOn: Date today.
	calendar
		center: calendar cursorPoint;
		fitInWorld;
		onChoiceSend: #onDateChosen: to: self.