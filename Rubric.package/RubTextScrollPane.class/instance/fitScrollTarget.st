scrollbar managing
fitScrollTarget
	"if the scroller extent is different that the textArea 
	extent, then, if the textArea is wrapped, then resize 
	the textArea width accordingly
	If the textArea is not wrapped, then do nothing
	"
	
	self wrapped
		ifFalse: [ ^ self ].
	self fitScrollTargetWidth 