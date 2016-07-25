desktop
desktopLogoChanged
	self showDesktopLogo
		ifFalse: [self setDesktopLogoWith: nil.
			^ self].
	self desktopLogoFileName
		ifEmpty: [self setDesktopLogoWith: self pharoLogo]
		ifNotEmpty: [[self
				setDesktopLogoWith: (ImageMorph withForm: (Form fromFileNamed: self desktopLogoFileName))]
				on: Error
				do: [self setDesktopLogoWith: self pharoLogo]].
