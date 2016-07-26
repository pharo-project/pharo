events
outerPortEvent: aPortEvent  
	super outerPortEvent: aPortEvent.	
	aPortEvent hasChanged ifTrue: [
		(self transmissions select: [:each | 
			(each originatesAt: aPortEvent port) and: [
				( aPortEvent transmissionContext includesPort: each destination) not ]]) 
			do: [:each | each transmitIn: aPortEvent transmissionContext copy ]]