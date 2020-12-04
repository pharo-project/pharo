Extension { #name : #MenuMorph }

{ #category : #'*GT-Spotter-Processors' }
MenuMorph >> withAllLeafItemsDo: aBlockClosure [ 
	
	self items do: [:anItem | anItem withAllLeafItemsDo: aBlockClosure]
]
