Extension { #name : 'ClyToolContext' }

{ #category : '*Commander-Calypso-Compatibility' }
ClyToolContext >> commanderCommandClasses [
	"Commands used to be added to the context menus of a context as every subclass of the classes I answered. They are declared with the <calypsoCommands> pragma now (see ClyCommandMap), so I answer nothing. I am kept for the code which still passes me to ClyCommandGroupBuilder."

	^ #(  )
]
