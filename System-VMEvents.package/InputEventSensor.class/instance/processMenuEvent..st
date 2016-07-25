private events
processMenuEvent: evt
	| handler localCopyOfEvt |

	Smalltalk globals at: #HostSystemMenus ifPresent: [ :menus |		
		localCopyOfEvt := evt shallowCopy.
		handler := (menus
			defaultMenuBarForWindowIndex: (localCopyOfEvt at: 8))
			getHandlerForMenu: (localCopyOfEvt at: 3) item: (localCopyOfEvt at: 4).
		handler handler value: localCopyOfEvt ]