building
registerActions

	self when: GLMBrowserClosing send: #actOnBrowserClosing: to: self.
	self when: DebugActionExecuted send: #updateBrowserFrom: to: self.
	self when: GLMPaneRemoved send: #actOnInspectorPaneRemoved: to: self.
