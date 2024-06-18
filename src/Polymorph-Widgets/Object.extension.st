Extension { #name : 'Object' }

{ #category : '*Polymorph-Widgets' }
Object >> iconFormSetNamed: aSymbol [

	^ Smalltalk ui icons iconFormSetNamed: aSymbol
]

{ #category : '*Polymorph-Widgets' }
Object >> iconNamed: aSymbol [

	^ Smalltalk ui icons iconNamed: aSymbol
]

{ #category : '*Polymorph-Widgets' }
Object >> okToClose [
	"Sent to models when a window closing.
	Allows this check to be independent of okToChange."

	^true
]

{ #category : '*Polymorph-Widgets' }
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

{ #category : '*Polymorph-Widgets' }
Object >> systemIconName [

	^ #class
]

{ #category : '*Polymorph-Widgets' }
Object class >> taskbarLabel [
	"Answer the label string for the receiver in a task bar
	or nil for the default."

	^nil
]

{ #category : '*Polymorph-Widgets' }
Object >> taskbarLabel [
	"Answer the label string for the receiver in a task bar
	or nil for the default."

	^self class taskbarLabel
]
