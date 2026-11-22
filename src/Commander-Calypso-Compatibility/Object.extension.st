Extension { #name : 'Object' }

{ #category : '*Commander-Calypso-Compatibility' }
Object class >> asCalypsoItemContext [
	^ClyBrowserItemContext itemType: self
]
