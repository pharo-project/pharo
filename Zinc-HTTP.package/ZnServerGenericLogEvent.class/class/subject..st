instance creation
subject: object
	"self subject: #(i 10 j -1)."
	"self subject: 'How are you?'."
	
	^ self new
		subject: object;
		yourself