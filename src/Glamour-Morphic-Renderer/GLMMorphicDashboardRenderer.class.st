Class {
	#name : #GLMMorphicDashboardRenderer,
	#superclass : #GLMMorphicBrowserRenderer,
	#category : 'Glamour-Morphic-Renderer'
}

{ #category : #private }
GLMMorphicDashboardRenderer >> render: aBrowser [
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
]

{ #category : #private }
GLMMorphicDashboardRenderer >> renderPane: aPane inUI: aMorph from: aDashboard [
	aMorph addMorph: (self renderObject: aPane) fullFrame: (aDashboard layoutOfPaneNamed: aPane name)
]
