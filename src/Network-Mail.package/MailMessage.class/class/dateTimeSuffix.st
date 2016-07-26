utilities
dateTimeSuffix
	"Answer a string which indicates the date and time, intended for use in building fileout filenames, etc."

	^self monthDayTime24StringFrom: Time primUTCSecondsClock