initialization
initialize

	super initialize.
	
	self changeProportionalLayout.
	
	state := TabEnabled tab: self.
	closeable := true.
	label := self buildLabel: 'tab' withIcon: self defaultIcon.
	closeIcon := AlphaImageMorph withForm: self class closeIcon.
	morph := nil.
	actions := OrderedCollection new.
	retrievingSemaphore := Semaphore new.
	
	self addLabel.
	self addIcon.
	self addActionsToIcon.