example
example
	| tc testMorph model model2 model3 anotherMorph anotherMorph2 associations classes |
	model := ClassMethodBrowser new.
	model2 := ClassMethodBrowser new.
	model3 := ClassMethodBrowser new.
	testMorph := model buildWithSpec.
	anotherMorph := model2 buildWithSpec.
	anotherMorph2 := model3 buildWithSpec.
	associations := {('Test' -> testMorph).
	('Another' -> anotherMorph).
	('Again' -> anotherMorph2)}.
	tc := self withTabs: associations.
	classes := Smalltalk globals allClasses.
	model classes: classes.
	model2 classes: classes.
	model3 classes: classes.
	(tc embeddedInMorphicWindowLabeled: 'test') openInWorld.
	^ tc