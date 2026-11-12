Extension { #name : 'ClyBrowserItemContext' }

{ #category : '*Commander-Calypso-Compatibility' }
ClyBrowserItemContext >> commandAnchor [
	"The anchor of ClyCommandMap that describes the same contexts as me, used to convert class annotations into declarations."

	^ itemType
]
