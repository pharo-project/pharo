utility
morphicTabsFor: aManagerModel
	"Retrieve all tabs in the morphic world to test whether they are being properly added/removed. It is assumed that the Model has already been built."

	^ (aManagerModel widget widget submorphs detect: [ :each | each class = TabBarMorph ]) tabs