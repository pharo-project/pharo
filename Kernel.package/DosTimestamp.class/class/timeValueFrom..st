private
timeValueFrom: aTime

	| dosHours dosMinutes dosSeconds |
	dosHours := aTime hour << 11.
	dosMinutes := aTime minute << 5.
	dosSeconds := (aTime second / 2) floor.
	^ dosHours + dosMinutes + dosSeconds.