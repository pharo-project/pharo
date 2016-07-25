utilities
monthDayTime24StringFrom: aSecondCount
	| aDate aTime |
	"From the date/time represented by aSecondCount, produce a string which indicates the date and time in the compact form
             ddMMMhhmm		where dd is a two-digit day-of-month, MMM is the alpha month abbreviation and hhmm is the time on a 24-hr clock.

          Utilities monthDayTime24StringFrom: Time primSecondsClock
"

	aDate := Date fromSeconds: aSecondCount.
	aTime := Time fromSeconds: aSecondCount \\ 86400.

	^ (aDate dayOfMonth asTwoCharacterString), 
		(aDate monthName copyFrom: 1 to: 3), 
		(aTime hhmm24)