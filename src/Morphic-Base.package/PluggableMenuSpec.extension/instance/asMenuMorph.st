*Morphic-Base
asMenuMorph
	| prior menu myitems |
	prior := parentMenu.
	parentMenu := menu := self morphClass new.
	self label ifNotNil: [ 
		parentMenu buildTitle: [ :titleMorph | 
			titleMorph bigTitle: self label 
		]
	].
	myitems := self items.
	myitems do: [ :each | each asMenuItemMorphFrom: parentMenu isLast: each = myitems last ].
	parentMenu := prior.
	^ menu