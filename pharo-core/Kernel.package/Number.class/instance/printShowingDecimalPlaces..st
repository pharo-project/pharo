printing
printShowingDecimalPlaces: placesDesired
	"Print the receiver showing precisely the given number of places desired.  If placesDesired is positive, a decimal point and that many digits after the decimal point will always be shown.  If placesDesired is zero, a whole number will be shown, without a decimal point."

	^String new: placesDesired + 10 streamContents: [:aStream |
		self printOn: aStream showingDecimalPlaces: placesDesired]
"
23 printShowingDecimalPlaces: 2
23.5698 printShowingDecimalPlaces: 2
-234.567 printShowingDecimalPlaces: 5
23.4567 printShowingDecimalPlaces: 0
23.5567 printShowingDecimalPlaces: 0
-23.4567 printShowingDecimalPlaces: 0
-23.5567 printShowingDecimalPlaces: 0
100000000 printShowingDecimalPlaces: 1
0.98 printShowingDecimalPlaces: 5
-0.98 printShowingDecimalPlaces: 2
2.567 printShowingDecimalPlaces: 2
-2.567 printShowingDecimalPlaces: 2
0 printShowingDecimalPlaces: 2
"