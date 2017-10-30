accessing
date: aDate

	| dateString |
	date := aDate.
	dateString := self displayBlock value: date.
	self dateModel text: dateString.