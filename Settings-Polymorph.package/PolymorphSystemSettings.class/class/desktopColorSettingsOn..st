desktop
desktopColorSettingsOn: aBuilder 
	(aBuilder setting: #desktopColor) label: 'Color';
		 description: 'This is the only color of your desktop if no gradient is used. If you make use of a gradient, then this color is the first one of the gradient';
		with: [(aBuilder setting: #useDesktopGradientFill) label: 'Gradient';
				 description: 'If true, then more settings will be available in order to define the desktop background color gradient';
				 noOrdering;
				with: [(aBuilder setting: #desktopGradientFillColor) label: 'Other color';
						 description: 'This is the second color of your gradient (the first one is given by the "Color" setting'.
					(aBuilder pickOne: #desktopGradientDirection) label: 'Direction';
						 domainValues: {#Horizontal. #Vertical. #Radial}.
					(aBuilder pickOne: #desktopGradientOrigin) label: 'Origin';
						 domainValues: {
							'Top left' translated -> #topLeft. 
							'Top' translated -> #topCenter. 
							'Top right' translated -> #topRight. 
							'Right' translated -> #rightCenter. 
							'Bottom right' translated -> #bottomRight. 
							'Bottom' translated -> #bottomCenter. 
							'Bottom left' translated -> #bottomLeft. 
							'Left' translated -> #leftCenter. 
							'Center' translated -> #center}]]