editing
acceptContents
	"The message is sent when the user hits enter or Cmd-S.
	Accept the current contents and endediting"
	self editingMode acceptAllowed ifFalse: [ ^self ].
	self announce: (RubTextAcceptRequest morph: self).
	self changed