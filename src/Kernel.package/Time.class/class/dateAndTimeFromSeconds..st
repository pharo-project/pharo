accessing
dateAndTimeFromSeconds: secondCountSinceEpoch
 
	| dateAndTime |
	dateAndTime := DateAndTime fromSeconds: secondCountSinceEpoch.
 	^ Array
 		with: dateAndTime asDate
 		with: dateAndTime asTime