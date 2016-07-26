running
setUp
	| text |
	text := self newText: self textForView1.
	view1 := self newViewOn:  text .
	
	text := self newText: self textForView2.
	view2 := self newViewOn:  text .
