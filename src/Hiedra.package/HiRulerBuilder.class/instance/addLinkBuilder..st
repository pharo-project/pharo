accessing - building API
addLinkBuilder: aBlockClosure 
	
	| newLink |
	newLink := HiLinkBuilder new.
	aBlockClosure value: newLink.
	linkBuilders add: newLink.