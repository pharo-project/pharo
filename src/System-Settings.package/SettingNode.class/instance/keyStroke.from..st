menu
keyStroke: anEvent from: aTreeMorph
	| c |
	c := anEvent keyCharacter.
	c = $b
		ifTrue: [self menuBrowse. ^ true].
	c = $i
		ifTrue: [self inspectItem. ^ true].
	c = $I
		ifTrue: [self exploreItem. ^ true].
	^ false
