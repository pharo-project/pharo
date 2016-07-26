interface building
newFontPreviewInnerPanel
	"Answer a morph for the preview text."
	
	| textMorph |
	textMorph := self newText: self previewText.
	textMorph margins: 4.
	^ self model selectedFont
		ifNil: [ textMorph ]
		ifNotNil: [ :f | textMorph beAllFont: f ]