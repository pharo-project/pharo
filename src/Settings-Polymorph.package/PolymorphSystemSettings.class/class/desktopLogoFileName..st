desktop
desktopLogoFileName: aFileName
	DesktopLogoFileName = aFileName ifTrue: [^ self].
	DesktopLogoFileName := aFileName.
	self desktopLogoChanged 