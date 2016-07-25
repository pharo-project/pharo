text managing
acceptEdits
	"Accept the current contents and end editing."
	self announce: RubTextAcceptedInModel new.
	self clearUserEdits 