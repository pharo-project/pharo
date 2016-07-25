non-interactive
nonInteractiveManager
	" Answer an instance of non-interactive manager, which will be used when image runs headless.
	  We put it here, so subclasses can override it. "
	^ NonInteractiveUIManager replacing: self