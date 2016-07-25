width
width: widths
	"The widths property can have from one to four values:
	
	width: #(25 50 75 100)
		top width is 25px
		right width is 50px
		bottom width is 75px
		left width is 100px
		
	width: #(25 50 75)
		top width is 25px
		right and left widths are 50px
		bottom width is 75px
		
	width: #(25 50)
		top and bottom widths are 25px
		right and left widths are 50px
		
	width: #(25)
		all four widths are 25px
		
	width: 25
		all four widths are 25px"
	
	widths ifNil: [ ^ self width: #(0) ].
	
	widths isCollection ifTrue: [
		widths isEmpty ifTrue: [ ^ self width: #(0) ].
		self
			widthTop: (widths at: 1);
			widthRight: (widths at: 1);
			widthBottom: (widths at: 1);
			widthLeft: (widths at: 1).
		widths size > 1 ifTrue: [
			self
				widthRight: (widths at: 2);
				widthLeft: (widths at: 2) ].
		widths size > 2 ifTrue: [
			self widthBottom: (widths at: 3) ].
		widths size > 3 ifTrue: [
			self widthLeft: (widths at: 4) ] ]
	ifFalse: [ ^ self width: {widths} ]