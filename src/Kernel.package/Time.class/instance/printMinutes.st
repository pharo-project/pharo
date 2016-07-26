printing
printMinutes
 	"Return as string 'hh:mm pm'  "
 
 	^ String new: 8 streamContents: [ :aStream | 
		self print24: false showSeconds: false on: aStream ]
 