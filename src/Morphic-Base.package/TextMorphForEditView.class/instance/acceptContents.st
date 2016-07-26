editing
acceptContents
	"The message is sent when the user hits enter or Cmd-S.
	Accept the current contents and end editing."
	self updateFromParagraph.
	editView accept.