accessing
pane
	^ pane ifNil: [
		pane := (GLMPane named: 'root' in: GLMNoBrowser new) 
				addPresentationSilently: self; 
				yourself]