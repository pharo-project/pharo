menu
addYellowButtonMenuItemsTo: aMenu event: evt 
	"Populate aMenu with appropriate menu items for a  
	yellow-button (context menu) click."
	aMenu defaultTarget: self.
	self defaultYellowButtonMenuEnabled ifFalse: [^ self].
	"aMenu addStayUpItem."
	self addModelYellowButtonItemsTo: aMenu event: evt.
	self cmdGesturesEnabled ifTrue: [
			aMenu addLine.
			aMenu add: 'inspect' selector: #inspect].
	aMenu addLine.
	self isWorldMorph ifFalse: [aMenu add: 'delete' selector: #delete].
	self world selectedObject == self
		ifTrue: [aMenu add: 'halo off' selector: #removeHalo]
		ifFalse: [aMenu add: 'halo on' selector: #addHalo].
	(self isWorldMorph
			or: [self wantsToBeTopmost])
		ifFalse: [
			aMenu addLine.
			aMenu add: 'send to back' selector: #goBehind.
			aMenu add: 'bring to front' selector: #comeToFront.
			self addEmbeddingMenuItemsTo: aMenu hand: evt hand].
	self isWorldMorph ifFalse: [
			self isFullOnScreen ifFalse: [aMenu add: 'move onscreen' selector: #goHome]].
			self addLayoutMenuItems: aMenu hand: evt hand.
			(owner notNil
					and: [owner isTextMorph])
				ifTrue: [self addTextAnchorMenuItems: aMenu hand: evt hand].
	self isWorldMorph ifFalse: [
			aMenu addLine.
			self addToggleItemsToHaloMenu: aMenu].
	aMenu addLine.
	self isWorldMorph
		ifFalse: [aMenu add: 'copy to paste buffer' selector: #copyToPasteBuffer:].
	(self allStringsAfter: nil) isEmpty ifFalse: [aMenu add: 'copy text' selector: #clipText].
	self addExportMenuItems: aMenu hand: evt hand.
	aMenu addLine.
	aMenu add: 'adhere to edge...' selector: #adhereToEdge.
	self addCustomMenuItems: aMenu hand: evt hand