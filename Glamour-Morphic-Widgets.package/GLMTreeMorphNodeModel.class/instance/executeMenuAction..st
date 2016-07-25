menu
executeMenuAction: anAction
	self containerTree announce: (GLMMenuItemSelected action: anAction)