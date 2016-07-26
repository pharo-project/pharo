accessing
headlessManager
	" Answer an instance of headless manager, which will be used when image runs headless and non-interactive.
	  We put it here, so subclasses can override it. "
	^ CommandLineUIManager replacing: self