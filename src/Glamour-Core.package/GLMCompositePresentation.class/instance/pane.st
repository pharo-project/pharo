accessing
pane
	^ pane ifNil: [
		pane := (GLMPane named: 'root' in: GLMNoBrowser new) 
				presentations: self; 
				yourself]