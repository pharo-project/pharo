shout
shoutAboutToStyle: myShoutStyler
	^ self model isNil
		or: [ 
			(self model interactionModel respondsTo: #shoutAboutToStyle:) not
				or: [ self model interactionModel shoutAboutToStyle: self ] ] 