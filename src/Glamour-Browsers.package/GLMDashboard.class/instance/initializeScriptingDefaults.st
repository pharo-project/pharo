scripting
initializeScriptingDefaults
	super initializeScriptingDefaults.
	self transmissions
		detect: [ :each | each originatesAt: (self pane port: #entity) ]
		ifNone: [ 
			self
				transmit to: (self panes first port: #entity);
				from: #outer port: #entity;
				andShow: [:a |  a custom: GLMFlexiblePresentation new ] ]