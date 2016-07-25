initialization
initialize
	"Initializes the receiver"
	
	super initialize.
	translations := Dictionary new.
	translations at: #'TestCase' put: 'Testfall'