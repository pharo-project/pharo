This morph manages a set of PluggableTabButtonMorphs.  Each tab should be added in the left to right order that they should be displayed.  Each tab will be evenly sized to fit the available space.  This morph intercepts mouse clicks, figures out which tab was clicked, pops up the new tab as the active tab and triggers the registered event.  See PluggableTabButtonMorph for information on what a tab can consist of.

Example:

(PluggableTabBarMorph on: nil)
	addTab: (Text fromString: 'Test') withAction: [Transcript show: 'Test'; cr];
	addTab: (Text fromString: 'Another') withAction: [Transcript show: 'Another'; cr];
	width: 200;
	openInHand
