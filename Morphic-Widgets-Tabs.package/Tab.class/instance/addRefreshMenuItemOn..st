private
addRefreshMenuItemOn: aMenu
	"I have a block for retrieving my contents, so I install a menu action for this"
	
	retrievingBlock ifNil: [ ^ self ].
	aMenu addLine.
	aMenu
		add: 'Refresh content'
		target: self
		selector: #refreshContent