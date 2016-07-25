rendering
registerAnnouncementsFor: aBrowser
	aBrowser when: GLMPaneRemoved send: #actOnPaneRemoved: to: self.
	aBrowser when: GLMPaneAdded send: #actOnPaneAdded: to: self.
	aBrowser when: GLMPaneReplaced send: #actOnPaneReplaced: to: self.
	aBrowser when: GLMBrowserClosed send: #actOnBrowserClosed: to: self