tabs
tabsAndControls
	"Answer the tabs and scrolling controls that should be visible given the space."

	|visibleTabs firstIndex|
	self tabs ifEmpty: [
		self visibleTabRange: (1 to: 0).
		^#()].
	visibleTabs := self calculateVisibleTabs.
	visibleTabs ifEmpty: [
		self visibleTabRange: (1 to: 0).
		^#()].
	firstIndex := self tabs indexOf: visibleTabs first.
	self visibleTabRange: (firstIndex to: firstIndex + visibleTabs size - 1).
	visibleTabs size = self tabs size ifFalse: [
		visibleTabs
			add: self newSpacer;
			addAll: self controls.
		self cornerStyle = #rounded
			ifTrue: [visibleTabs add: self newEndSpacer]].
	^visibleTabs