menu commands
cancel
	(self confirm:
'This action will cancel your changes.
Is it OK to cancel changes?' translated) ifTrue: [self setText: self getText.
	self setSelection: self getSelection.]