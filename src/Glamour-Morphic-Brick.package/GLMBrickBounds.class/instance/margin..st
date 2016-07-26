accessing - margin/padding
margin: margins
	"The margins property can have from one to four values:
	
	margin: #(25 50 75 100)
		top margin is 25px
		right margin is 50px
		bottom margin is 75px
		left margin is 100px
		
	margin: #(25 50 75)
		top margin is 25px
		right and left margins are 50px
		bottom margin is 75px
		
	margin: #(25 50)
		top and bottom margins are 25px
		right and left margins are 50px
		
	margin: #(25)
		all four margins are 25px
		
	margin: 25
		all four margins are 25px"
	
	margins ifNil: [ ^ self margin: #(0) ].
	
	margins isCollection ifTrue: [
		margins isEmpty ifTrue: [ ^ self margin: #(0) ].
		self
			marginTop: (margins at: 1);
			marginRight: (margins at: 1);
			marginBottom: (margins at: 1);
			marginLeft: (margins at: 1).
		margins size > 1 ifTrue: [
			self
				marginRight: (margins at: 2);
				marginLeft: (margins at: 2) ].
		margins size > 2 ifTrue: [
			self marginBottom: (margins at: 3) ].
		margins size > 3 ifTrue: [
			self marginLeft: (margins at: 4) ] ]
	ifFalse: [ ^ self margin: {margins} ].
	
	self updateCache