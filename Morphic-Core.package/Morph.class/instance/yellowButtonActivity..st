event handling
yellowButtonActivity: shiftState 
	"Find me or my outermost owner that has items to add to a  
	yellow button menu.  
	shiftState is true if the shift was pressed.  
	Otherwise, build a menu that contains the contributions from  
	myself and my interested submorphs,  
	and present it to the user."
	| menu |
	self isWorldMorph
		ifFalse: [| outerOwner | 
			outerOwner := self outermostOwnerWithYellowButtonMenu.
			outerOwner
				ifNil: [^ false].
			outerOwner == self
				ifFalse: [^ outerOwner yellowButtonActivity: shiftState]].
	menu := self buildYellowButtonMenu: ActiveHand.
	menu
		addTitle: self externalName
		icon: (self iconOrThumbnailOfSize: 28).
	menu popUpInWorld: self currentWorld.
	^ true