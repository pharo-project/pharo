installing
install

	self isInstalled ifFalse: [
		Nautilus pluginClasses:
			(Nautilus pluginClasses copyWith:
				{ self . self defaultPosition }) ]