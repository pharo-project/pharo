new selection
notify: aString at: anInteger in: aStream 
	^ self notificationStrategy 
		notify: aString 
		at: anInteger 
		in: aStream 
