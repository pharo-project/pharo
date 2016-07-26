drawing
render 

	surface drawDuring: [:canvas | 
		surface clear.
		canvas pathTransform loadIdentity.
		canvas setPaint: Color lightGray;
		drawShape: (0@0 corner: self extent).
		self perform: (self figures at: current)  with: canvas
	].
