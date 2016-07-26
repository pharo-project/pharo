examples
"protocol: examples"
example
	|foundation scrollPane|
	
	scrollPane := self new.
	
	foundation := GLMFoundationBrick new.
	foundation brickRoot: scrollPane.
	
	1 to: 20 do: [ :index |
		scrollPane addBrickBack: (GLMBrick new
			color: Color random;
			hSpaceFill;
			height: 50).
	].
	
	foundation extent: 200@200.
	
	foundation openInWindow