menu actions
makeSecondTopmost
	| aWorld |
	aWorld := self world.
	aWorld submorphs
		detect: [ :m | m isSystemWindow and: [ m ~~ self ] ]
		ifFound: [ :nextWindow | 
			nextWindow activate.
			aWorld addMorph: self behind: nextWindow ]