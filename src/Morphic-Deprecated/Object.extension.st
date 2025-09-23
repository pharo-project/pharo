Extension { #name : 'Object' }

{ #category : '*Morphic-Deprecated' }
Object >> systemIcon [
	"dont use this method. It will be deprecated.
	Tools should resolve real icon object based on icon name"

	self
		deprecated: 'The direct usage of #systemIcon is not recommended anymore since it
	introduces a dependency with UI layer we do not want to have here. Instead, our UI
	components should us #systemIconName to get the icon to use, and then user can resolve it
	as their want (since different applications can have different iconpacks, etc.) #Pharo11'
		transformWith: '`@receiver systemIcon' -> '`@receiver iconNamed: `@receiver systemIconName'.

	^ self iconNamed: self systemIconName
]
