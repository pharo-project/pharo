accessing
promptForCancel
	"Ask if it is OK to cancel changes"
	
	^(self confirm:
'Changes have not been saved.
Is it OK to cancel changes?' translated)
			