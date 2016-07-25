keymapping protocol
platform: anArray
 
	anArray isEmpty
		ifTrue: [ platform := #all ]
		ifFalse: [ platform := anArray first ]