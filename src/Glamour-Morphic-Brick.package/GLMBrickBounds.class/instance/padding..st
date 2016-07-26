accessing - margin/padding
padding: paddings
	"The paddings property can have from one to four values:
	
	padding: #(25 50 75 100)
		top padding is 25px
		right padding is 50px
		bottom padding is 75px
		left padding is 100px
		
	padding: #(25 50 75)
		top padding is 25px
		right and left paddings are 50px
		bottom padding is 75px
		
	padding: #(25 50)
		top and bottom paddings are 25px
		right and left paddings are 50px
		
	padding: #(25)
		all four paddings are 25px
		
	padding: 25
		all four paddings are 25px"
	
	paddings ifNil: [ ^ self padding: #(0) ].
	
	paddings isCollection ifTrue: [
		paddings isEmpty ifTrue: [ ^ self padding: #(0) ].
		self
			paddingTop: (paddings at: Top);
			paddingRight: (paddings at: Top);
			paddingBottom: (paddings at: Top);
			paddingLeft: (paddings at: Top).
		paddings size > Top ifTrue: [
			self
				paddingRight: (paddings at: Right);
				paddingLeft: (paddings at: Right) ].
		paddings size > Right ifTrue: [
			self paddingBottom: (paddings at: Bottom) ].
		paddings size > Bottom ifTrue: [
			self paddingLeft: (paddings at: Left) ] ]
	ifFalse: [ ^ self padding: { paddings } ].
	self updateCache