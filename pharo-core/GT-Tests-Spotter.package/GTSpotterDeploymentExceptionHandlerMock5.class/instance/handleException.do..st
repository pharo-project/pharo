public
handleException: anException do: aBlock
	super handleException: anException do: aBlock. " we perform the block because mock5 should not raise any exception within aBlock "
	^ aBlock value