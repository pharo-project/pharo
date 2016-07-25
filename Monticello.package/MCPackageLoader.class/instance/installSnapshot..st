public
installSnapshot: aSnapshot
	| patch |
	patch := aSnapshot patchRelativeToBase: MCSnapshot empty.
	patch applyTo: self.
