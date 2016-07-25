instance creation
starting: aDateAndTime duration: aDuration

	^ self basicNew
  		start: aDateAndTime asDateAndTime;
 		duration: aDuration;
		yourself.