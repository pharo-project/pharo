event-handling
updateList
	"update the list of elements displayed according to the new object"

	| elements |
"	self haltOnce."
	elements := self generateElements.	
	self list listItems = elements ifTrue: [ ^ self ].
	"first reset the items to make sure we don't interfere with the display block"
	"self list items: #()."
	self list displayBlock: [ :eyeElement | self labelFor: eyeElement ].
	self list items: elements.
	
	"handle when last item of list is removed"
	(self list selectedIndex > self list listItems size) ifTrue: [ self list setSelectedIndex: self list listItems size  ].
	
	"handle when selected dictionary key is removed"
	self list setSelectedIndex: self list selectedIndex.
	
	self list menu: [ :aMenu | self inspectionMenu: aMenu ]