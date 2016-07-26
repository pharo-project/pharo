lazy scrolling
updateVisible
	|invisible|
	
	self model ifNil: [ ^ self ].
	self model count = 0 ifTrue: [ ^ self ].
	
	invisible := self gatherInvisibleItems.
	invisible = self band subbricks & invisible isEmpty not ifTrue: [ ^ self ].
	
	self updateVisibleTop: invisible.
	self updateVisibleBottom: invisible.
	
	invisible do: [ :each | self removeBrickNoLayout: each ].
	
	visibleIndex := self getIndex: self band firstSubbrick 