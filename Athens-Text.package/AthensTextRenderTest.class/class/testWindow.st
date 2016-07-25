as yet unclassified
testWindow

	^ (AthensPluggableTextMorph
		on: self
		text: #testText
		accept: nil) embeddedInMorphicWindowLabeled: 'A fancy looking title text'
