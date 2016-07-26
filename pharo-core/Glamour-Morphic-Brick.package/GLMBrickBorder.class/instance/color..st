color
color: colors
	"The colors property may have from one to four values:
	
	color: { Color red . Color blue . Color green . Color yellow }
		top color is red
		right color is blue
		bottom color is green
		left color is yellow
		
	color: { Color red . Color blue . Color green }
		top color is red
		right and left colors are blue
		bottom color is green
		
	color: { Color red . Color blue }
		top and bottom colors are red
		right and left colors are blue
		
	color: { Color red }
		all four colors are red
		
	color: Color red
		all four colors are red"
	
	colors ifNil: [ ^ self color: #(0) ].
	
	colors isCollection ifTrue: [
		colors isEmpty ifTrue: [ ^ self color: #(0) ].
		self
			colorTop: (colors at: 1);
			colorRight: (colors at: 1);
			colorBottom: (colors at: 1);
			colorLeft: (colors at: 1).
		colors size > 1 ifTrue: [
			self
				colorRight: (colors at: 2);
				colorLeft: (colors at: 2) ].
		colors size > 2 ifTrue: [
			self colorBottom: (colors at: 3) ].
		colors size > 3 ifTrue: [
			self colorLeft: (colors at: 4) ] ]
	ifFalse: [ ^ self color: { colors } ].