initialization
signature: signature "<String>" block: aBlock "<BlockClosure>"
	super signature: signature block: (self wrapBlock: aBlock)