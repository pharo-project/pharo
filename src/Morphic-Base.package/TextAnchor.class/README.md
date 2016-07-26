TextAnchors support anchoring of images in text.  A TextAnchor exists as an attribute of a special text placeholder - "(Character value: 1) asText".  Depending on whether its anchoredMorph is a Morph or a Form, it repositions the morph, or displays the form respectively.  The coordination between composition, display and selection can best be understood by browsing the various implementations of placeEmbeddedObject:.

In the morphic world, simply embed any form or morph in text.

	| textWithForm |
	textWithForm := (Text withAll: 'Text with -->'), (Text string: (String value: 1) attribute: (TextAnchor new anchoredMorph: ThemeIcons current homeIcon)), (Text withAll: '<-- embedded Form').
	textWithForm asMorph openInHand.

	| textWithMorph |
	textWithMorph := (Text withAll: 'Text with -->'), (Text string: (String value: 1) attribute: (TextAnchor new anchoredMorph: EllipseMorph new)), (Text withAll: '<-- embedded Morph').
	textWithMorph asMorph openInHand.