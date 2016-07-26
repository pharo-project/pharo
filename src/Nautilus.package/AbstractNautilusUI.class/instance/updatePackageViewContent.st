private
updatePackageViewContent

	| vScrollValue |
	vScrollValue := self packageWidget scrollValue y.
	self changed: #getPackageItem:.
	self packageWidget vScrollValue: vScrollValue