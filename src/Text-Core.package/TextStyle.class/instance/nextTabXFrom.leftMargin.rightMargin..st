tabs and margins
nextTabXFrom: anX leftMargin: leftMargin rightMargin: rightMargin 
	"Tab stops are distances from the left margin. Set the distance into the 
	argument, anX, normalized for the paragraph's left margin."
	| normalizedX tabX |
	normalizedX := anX - leftMargin.
	1 
		to: tabsArray size
		do: 
			[ :i | 
			(tabX := tabsArray at: i) > normalizedX ifTrue: [ ^ leftMargin + tabX min: rightMargin ] ].
	^ rightMargin