typing/selecting keys
search: aKeyboardEvent
	"Invoked by Ctrl-S.  Same as 'again', but always uses the existing findText
	 from current FindReplaceService"

	self closeTypeIn.
	self findAgain.
	^true