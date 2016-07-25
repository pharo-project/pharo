class initialization
initialize
	" self initialize "
	PackagesIconsCache := WeakIdentityKeyDictionary new.
	
	NextFocusKey := Character arrowRight.
	PreviousFocusKey := Character arrowLeft.
	
	self registerSystemAnnouncements.