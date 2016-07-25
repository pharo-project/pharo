private
needSpaceForActions 
	| tab |
	
	tab := self selectedTab.
	tab ifNil: [ ^ false ].

	^ self selectedTab hasMenu or: [ self selectedTab hasActions ]