Morphic-Base-Windows
bringWindowsFullOnscreen
	"Make ever SystemWindow on the desktop be totally on-screen, whenever possible."

	| r |
	bounds
		allAreasOutsideList: (self taskbars collect: [ :t | t bounds ])
		do: [ :e | 
			r isNil
				ifTrue: [ r := e ]
				ifFalse: [ 
					r area > e area
						ifTrue: [ r ]
						ifFalse: [ e ] ] ].
	(self windowsSatisfying: [ :w | true ])
		do: [ :aWindow | 
			aWindow right: (aWindow right min: r right).
			aWindow bottom: (aWindow bottom min: r bottom).
			aWindow left: (aWindow left max: r left).
			aWindow top: (aWindow top max: r top) ]