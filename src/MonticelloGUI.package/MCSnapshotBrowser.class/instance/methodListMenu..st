menus
methodListMenu: aMenu 
	super methodListMenu: aMenu.
	( self selectedMessageName notNil and: [ methodSelection isLoadable ] )
		ifTrue: [ aMenu addLine; add: 'Load method' selector: #loadMethodSelection ].
	^ aMenu