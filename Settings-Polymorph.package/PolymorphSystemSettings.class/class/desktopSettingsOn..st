desktop
desktopSettingsOn: aBuilder 
	(aBuilder group: #desktopSettings)
		label: 'Desktop';
		target: self;
		noOrdering;
		with: [
			(aBuilder setting: #showDesktopLogo)
				label: 'Show logo' ;
				description: 'If true, then a logo if shown on the top left of the desktop' ;
				with: [
					(aBuilder setting: #desktopLogoFileName)
						type: #FilePathEncoder;
						description: 'The path of an image file for your own logo, the default pharo logo is used if empty.' ;
						label: 'Logo image file name'].
			self desktopColorSettingsOn: aBuilder.
			(aBuilder setting: #desktopImageFileName)
				type: #FilePathEncoder;
				description: 'The path of an image file which will be imported as a form and displayed as your desktop background';
				label: 'Desktop background image file name';
			with: [
				(aBuilder pickOne: #desktopImageLayout) label: 'Layout';
				 description: 'Size and placement of the background image';
				 target: self;
				 order: 0;
				 default: #scaled;
				 domainValues: #(#center #tiled #scaled #scaledAspect
		#topLeft #topCenter #topRight #rightCenter
		#bottomRight #bottomCenter #bottomLeft #leftCenter).
				]]
		
						