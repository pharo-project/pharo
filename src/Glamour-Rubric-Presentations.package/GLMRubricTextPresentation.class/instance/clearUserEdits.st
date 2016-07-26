actions text
clearUserEdits
	self flag: 'Should remove the need for using #defer:. Currently it used as
	the rendered suspends all announcements for a presentation when it handles a text changed request.'.
	UIManager default defer: [ 
		self announce: (GLMTextEditorRequest new 
			action: [ :anEditor | anEditor model clearUserEdits]) ]