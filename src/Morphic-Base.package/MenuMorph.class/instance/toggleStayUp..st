menu
toggleStayUp: evt
	"Toggle my 'stayUp' flag and adjust the menu item to reflect its new state."

	self items do: [:item |
		item isStayUpItem ifTrue:
			[self stayUp: stayUp not.	
			 stayUp
				ifTrue: [item contents: 'dismiss this menu']
				ifFalse: [item contents: 'keep this menu up']]].
	evt hand releaseMouseFocus: self.
	stayUp ifFalse: [self topRendererOrSelf delete].
