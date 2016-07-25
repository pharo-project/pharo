printing
printYMDOn: aStream
	"Print just YYYY-MM-DD part.
	If the year is negative, prints out '-YYYY-MM-DD'."

	^ self printYMDOn: aStream withLeadingSpace: false.
