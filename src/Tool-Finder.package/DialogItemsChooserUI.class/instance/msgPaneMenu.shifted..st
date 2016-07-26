private
msgPaneMenu: aMenu shifted: shifted
	| donorMenu |
	
	donorMenu := shifted
		ifTrue: [SmalltalkEditor shiftedYellowButtonMenu]
		ifFalse: [SmalltalkEditor yellowButtonMenu].
	^ aMenu addAllFrom: donorMenu