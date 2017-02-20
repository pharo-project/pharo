border-styles
configureWindowBorderFor: aWindow
"	super configureWindowBorderFor: aWindow.
	aWindow roundedCorners: #()"
	| aStyle |
	
	aStyle := 
		SimpleBorder new 
			color: self borderColor;
			width: 1.
			
	aWindow borderStyle: aStyle.