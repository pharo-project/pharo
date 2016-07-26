text managing
acceptEditsInView
	self reconfigureViewWith:  [ :view | 
		view textArea announce: (RubTextAcceptRequest morph: view textArea) ]