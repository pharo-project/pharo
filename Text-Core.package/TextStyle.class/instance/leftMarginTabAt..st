tabs and margins
leftMarginTabAt: marginIndex 
	"Set the 'nesting' level of left margin indents of the paragraph in the 
	style of the receiver to be the argument, marginIndex."

	(marginIndex > 0 and: [marginIndex < marginTabsArray size])
		ifTrue: [^(marginTabsArray at: marginIndex) at: 1]
		ifFalse: [^0]	
	"The marginTabsArray is an Array of tuples.  The Array is indexed according 
	to the marginIndex, the 'nesting' level of the requestor."
