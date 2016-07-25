keymapping protocol
category: aByteSymbol shortcut: aKMModifiedShortcut do: aBlockClosure 
	
	self currentItem
		category: aByteSymbol;
		default: aKMModifiedShortcut;
		action: aBlockClosure 