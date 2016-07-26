menu messages
accept: aKeyboardEvent 
	"Save the current text of the text being edited as the current acceptable version for purposes of canceling.  Allow my morph to take appropriate action"
	self accept.
	^ true