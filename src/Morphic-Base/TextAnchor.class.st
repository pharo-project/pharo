"
TextAnchors support anchoring of images in text.  A TextAnchor exists as an attribute of a special text placeholder - ""(Character value: 1) asText"".  Depending on whether its anchoredMorph is a Morph or a Form, it repositions the morph, or displays the form respectively.  The coordination between composition, display and selection can best be understood by browsing the various implementations of placeEmbeddedObject:.

In the morphic world, simply embed any form or morph in text.

	| textWithForm |
	textWithForm := (Text withAll: 'Text with -->'), (Text string: (String value: 1) attribute: (TextAnchor new anchoredMorph: ThemeIcons current homeIcon)), (Text withAll: '<-- embedded Form').
	textWithForm asMorph openInHand.

	| textWithMorph |
	textWithMorph := (Text withAll: 'Text with -->'), (Text string: (String value: 1) attribute: (TextAnchor new anchoredMorph: EllipseMorph new)), (Text withAll: '<-- embedded Morph').
	textWithMorph asMorph openInHand.
"
Class {
	#name : #TextAnchor,
	#superclass : #TextAttribute,
	#instVars : [
		'anchoredMorph'
	],
	#category : 'Morphic-Base-Text Support'
}

{ #category : #comparing }
TextAnchor >> = other [ 
	^ (other class == self class) 
		and: [other anchoredMorph == anchoredMorph]
]

{ #category : #accessing }
TextAnchor >> anchoredMorph [
	^ anchoredMorph
]

{ #category : #accessing }
TextAnchor >> anchoredMorph: aMorph [ 
	anchoredMorph := aMorph
]

{ #category : #visiting }
TextAnchor >> emphasizeScanner: scanner [
	"Do nothing for emphasizing the scanner - if the anchor is valid a #embeddedObject will be encountered by the scanner and do the real thing"
]

{ #category : #comparing }
TextAnchor >> hash [
	"#hash is re-implemented because #= is re-implemented"
	^anchoredMorph identityHash
]

{ #category : #accessing }
TextAnchor >> mayBeExtended [
	"A textAnchor is designed to modify only a single character, and therefore must not be extended by the ParagraphEditor's emphasisHere facility"
	^ false
]
