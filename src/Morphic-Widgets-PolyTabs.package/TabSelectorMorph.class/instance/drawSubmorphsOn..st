drawing
drawSubmorphsOn: aCanvas 
	"Display submorphs back to front.
	Draw the focus here since we are using inset bounds
	for the focus rectangle."

	|tab|
	super drawSubmorphsOn: aCanvas.
	(self hasKeyboardFocus and: [(tab := self selectedTab) notNil
			and: [tab owner notNil]])
		ifTrue: [self clipSubmorphs 
				ifTrue: [aCanvas
							clipBy: (aCanvas clipRect intersect: self clippingBounds ifNone:["done here" ^ self ])
							during: [:c | tab drawKeyboardFocusOn: c]]
				ifFalse: [tab drawKeyboardFocusOn: aCanvas]]