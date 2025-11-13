Extension { #name : 'Object' }

{ #category : '*Deprecated14' }
Object >> taskbarIcon [
	"Answer the icon for the receiver in a task bar
	or nil for the default."

	self
		deprecated: 'The direct usage of #taskbarIcon is not recommended anymore since it
	introduces a dependency with UI layer we do not want to have here. Instead, our UI
	components should us #taskbarIconName to get the icon to use, and then user can resolve it
	as their want (since different applications can have different iconpacks, etc.) #Pharo11'
		transformWith: '`@receiver taskbarIcon' -> '`@receiver iconNamed: `@receiver taskbarIconName'.

	^ self iconNamed: self taskbarIconName
]
