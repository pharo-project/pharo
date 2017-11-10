Extension { #name : #TabGroupMorph }

{ #category : #'*Polymorph-Widgets' }
TabGroupMorph >> themeChanged [
	"Update the corner style."

	self cornerStyle: (self theme tabGroupCornerStyleIn: self window).
	self pageMorphs do: [ :morph | morph themeChanged ].
	super themeChanged
]
