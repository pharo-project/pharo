border-styles
configureWindowBorderFor: aWindow
"	super configureWindowBorderFor: aWindow.
	aWindow roundedCorners: #()"
	| aStyle |
	
	aStyle := 
		SimpleBorder new 
			color: (Color lightGray);
			width: 1.
			
	aWindow borderStyle: aStyle.