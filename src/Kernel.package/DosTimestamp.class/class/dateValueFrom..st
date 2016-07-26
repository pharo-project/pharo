private
dateValueFrom: aDate

	| dosYear dosMonth dosDay |
	dosYear := aDate year - DateAndTime dosEpoch year << 9.
	dosMonth := aDate monthIndex << 5.
	dosDay := aDate dayOfMonth.
	^ dosYear + dosMonth + dosDay.