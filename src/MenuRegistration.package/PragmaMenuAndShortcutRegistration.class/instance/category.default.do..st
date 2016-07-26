keymapping protocol
category: aByteSymbol default: aKMModifiedShortcut do: aBlockClosure 
	
	self currentItem
		category: aByteSymbol;
		default: aKMModifiedShortcut;
		action: aBlockClosure 