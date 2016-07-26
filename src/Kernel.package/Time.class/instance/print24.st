printing
print24
 	"Return as 8-digit string 'hh:mm:ss', with leading zeros if needed"
 
 	^ String new: 8 streamContents: [ :aStream | 
		self print24: true on: aStream ]
 
 