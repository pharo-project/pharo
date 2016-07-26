keymapping protocol
category: aByteSymbol default: aKMModifiedShortcut do: aBlockClosure description: aByteString 
	
	self currentItem
		category: aByteSymbol;
		default: aKMModifiedShortcut;
		action: aBlockClosure;
		help: aByteString