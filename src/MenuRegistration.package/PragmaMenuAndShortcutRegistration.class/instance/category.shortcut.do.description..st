keymapping protocol
category: aByteSymbol shortcut: aKMModifiedShortcut do: aBlockClosure description: aByteString 
	
	self currentItem
		category: aByteSymbol;
		default: aKMModifiedShortcut;
		action: aBlockClosure;
		help: aByteString