instance-creation
newJoinSectionFrom: srcRange to: dstRange
	"Answer a new join section."

	|sourceParagraphLines destinationParagraphLines sourceTopPixelPosition sourceBottomPixelPosition destinationTopPixelPosition destinationBottomPixelPosition type rectangleColor|
	sourceParagraphLines := self srcMorph textMorph paragraph lines.
	destinationParagraphLines := self dstMorph textMorph paragraph lines.
	type := #modification.
	sourceTopPixelPosition := srcRange first > sourceParagraphLines size
		ifTrue: [type := #addition.
				sourceParagraphLines last bottom truncated - 1]
		ifFalse: [(sourceParagraphLines at: srcRange first) top truncated - 1].
	sourceBottomPixelPosition := srcRange size < 1
		ifTrue: [type := #addition.
				 sourceTopPixelPosition + 3]
		ifFalse: [srcRange last > sourceParagraphLines size
				ifTrue: [sourceParagraphLines last bottom truncated + 3]
				ifFalse: [(sourceParagraphLines at: srcRange last) bottom truncated - 1]].
	destinationTopPixelPosition := dstRange first > destinationParagraphLines size
		ifTrue: [type := #removal.
				destinationParagraphLines last bottom truncated - 1]
		ifFalse: [(destinationParagraphLines at: dstRange first) top truncated - 1].
	destinationBottomPixelPosition := dstRange size < 1
		ifTrue: [type := #removal.
				destinationTopPixelPosition + 3]
		ifFalse: [dstRange last > destinationParagraphLines size
				ifTrue: [destinationParagraphLines last bottom truncated + 3]
				ifFalse: [(destinationParagraphLines at: dstRange last) bottom truncated - 1]].
	rectangleColor := self colorForType: type.
	^self newJoinSection
		type: type;
		srcColor: rectangleColor;
		dstColor: rectangleColor;
		srcLineRange: srcRange;
		dstLineRange: dstRange;
		srcRange: (sourceTopPixelPosition to: sourceBottomPixelPosition);
		dstRange: (destinationTopPixelPosition to: destinationBottomPixelPosition);
		createHighlightsFrom: self srcMorph textMorph paragraph
		to: self dstMorph textMorph paragraph