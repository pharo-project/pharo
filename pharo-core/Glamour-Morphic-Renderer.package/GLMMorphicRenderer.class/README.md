This is the class responsible for the binding to Morphic.

| browser |
browser := GLMTabulator new.
browser column: #one; column: #two.

browser transmit to: #one; andShow: [:a | a list.].
browser transmit to: #two; from: #one; andShow: [ :a |
	a text.].
browser openOn: #(a b c d)