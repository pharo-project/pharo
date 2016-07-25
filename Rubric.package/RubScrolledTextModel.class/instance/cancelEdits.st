text managing
cancelEdits
	"Cancel the current contents and end editing."
	self announce: RubCancelEditRequestedInModel new.
	self clearUserEdits  