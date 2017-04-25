private
render: aBrowser
	| container |
	container := GLMPanelMorph new.
	container
		fillStyle: Color transparent;
		changeProportionalLayout;
		hResizing: #spaceFill;
		vResizing: #spaceFill.
	aBrowser panes reverseDo: [ :aGLMPane | self renderPane: aGLMPane inUI: container from: aBrowser ].
	aBrowser when: GLMBrowserClosed send: #actOnBrowserClosed: to: self.
	^ container