widget API
menu: aMenu
	| menuModel |
	
	menuModel := self model menu.
	menuModel isBlock ifTrue: [ menuModel := menuModel value ].
	^ menuModel buildWithSpec