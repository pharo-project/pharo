actions
mainPanel
	"Anwer the main panel morph or nil if not yet present."

	^self paneMorphs isEmpty
		ifFalse: [self paneMorphs first]