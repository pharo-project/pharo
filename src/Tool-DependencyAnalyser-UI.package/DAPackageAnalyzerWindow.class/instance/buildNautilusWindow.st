building
buildNautilusWindow
	| nautilusUI nautilusWindow |

	nautilusUI := NautilusUI on: nautilus.
	nautilus instVarNamed: 'ui' put: nautilusUI.
	nautilusWindow := NautilusWindow new model: nautilusUI.
	nautilusUI 
		addAll: nautilusWindow;
		window: nautilusWindow;
		setWindowTitle.
	^ nautilusWindow