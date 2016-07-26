*Manifest-Core
criticNameOn: aStream
	"This behavior may be folded later by changing the name of this method or using another one."
	
	aStream 
		<< self name 
		<< ' (' 
		<< self category 
		<< ')' 