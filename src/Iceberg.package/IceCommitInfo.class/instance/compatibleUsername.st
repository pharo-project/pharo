accessing
compatibleUsername
	"Timestamp of method has a very precise format that needs to be fulfiled: 
		
		'FirstnameLastname MM/DD/YYYY HH/MM' 
	
	Problem is that git username is different: Firstname Lastname.
	To make this compatible, we remove spaces... but this can be a problem in the future... 
	we'll see."
	^ self username reject: [ :each | each = Character space ]	