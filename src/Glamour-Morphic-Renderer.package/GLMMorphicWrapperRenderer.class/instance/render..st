rendering
render: aBrowser
	| paneMorph |
	container := GLMMorphic containerMorph.
	paneMorph := self renderObject: aBrowser defaultPane.
	paneMorph layoutFrame: (LayoutFrame identity).
	container addMorphBack: paneMorph.
	aBrowser when: GLMBrowserClosed send: #actOnBrowserClosed: to: self.
	^ container