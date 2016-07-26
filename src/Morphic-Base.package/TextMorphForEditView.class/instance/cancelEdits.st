editing
cancelEdits
	"The message is sent when the user hits enter or Cmd-L.
	Cancel the current contents and end editing."
	self releaseParagraph.
	editView cancel