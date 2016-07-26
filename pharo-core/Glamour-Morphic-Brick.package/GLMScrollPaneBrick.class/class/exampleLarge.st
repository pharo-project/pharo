examples
"protocol: examples"
exampleLarge
	|foundation scrollPane|
	
	scrollPane := self new.
	
	foundation := GLMFoundationBrick new.
	foundation brickRoot: scrollPane.
	
	1 to: 1000 do: [ :index |
		scrollPane addBrickBack: (GLMBrick new
			color: Color random;
			hSpaceFill;
			height: 50).
	].
	
	foundation extent: 400@300.
	
	foundation openInWindow