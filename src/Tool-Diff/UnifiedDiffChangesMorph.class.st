"
I am a Morph to display changes for visualising differences between two strings in a uniffied view.

I interact with StRewriteCritiqueChangesBrowserPresenter.


"
Class {
	#name : 'UnifiedDiffChangesMorph',
	#superclass : 'DiffMorph',
	#instVars : [
		'unifiedText',
		'copyDstMorph'
	],
	#category : 'Tool-Diff-Morphs',
	#package : 'Tool-Diff',
	#tag : 'Morphs'
}

{ #category : 'instance creation' }
UnifiedDiffChangesMorph class >> from: old to: new [
	"Answer a new instance of the receiver with the given
	old and new text."

	^self new showOnlyDestination: true;
		from: old
		to: new
]

{ #category : 'class initialization' }
UnifiedDiffChangesMorph class >> initialize [

	self deprecatedAliases: { #StUnifiedDiffChangesMorph }
]

{ #category : 'actions' }
UnifiedDiffChangesMorph >> calculatedJoinMappings [
	"Calculate the join parameters between src and dst and answer"

	| sourceLine destinationLine copyLine joins destinationRunStart sourceRunStart copyRunStart destinationRunEnd sourceRunEnd copyRunEnd matchDestinationStart matchSourceStart type lineRemove |
	type := #modification.
	lineRemove := 0.
	sourceLine := destinationLine := copyLine := 0.
	joins := OrderedCollection new.
	destinationRunStart := destinationRunEnd := sourceRunStart := sourceRunEnd := matchSourceStart := matchDestinationStart := copyRunStart := copyRunEnd := 0.
	self difference do: [ :p | 
			p key = #match
				ifTrue: [
					sourceLine := sourceLine + 1.
					destinationLine := destinationLine + 1.
					copyLine := copyLine + 1.
					matchSourceStart = 0
						ifTrue: [ 
							matchSourceStart := sourceLine.
							matchDestinationStart := destinationLine ].
					(destinationRunStart > 0 or: [ sourceRunStart > 0 ])
						ifTrue: [ 
							sourceRunStart = 0
								ifTrue: [ sourceRunStart := sourceLine ].
							destinationRunStart = 0
								ifTrue: [ destinationRunStart := destinationLine ].
							copyRunStart = 0
								ifTrue: [ copyRunStart := copyLine ].
							sourceRunEnd = 0
								ifTrue: [ sourceRunEnd := sourceRunStart - 1 ].
							destinationRunEnd = 0
								ifTrue: [ destinationRunEnd := destinationRunStart - 1 ].
							copyRunEnd = 0
								ifTrue: [ copyRunEnd := copyRunStart - 1 ].
							joins
								add:
									(self newJoinSectionFrom: (sourceRunStart to: sourceRunEnd) 
											to: (destinationRunStart to: destinationRunEnd) 
											copyRange: (copyRunStart to: copyRunEnd ) 
											withType: type
											flagRemove: (destinationRunEnd - destinationRunStart) - (sourceRunEnd - sourceRunStart) ).
							destinationRunStart := destinationRunEnd := sourceRunStart := sourceRunEnd := copyRunStart := copyRunEnd := 0 ] ].
			p key = #remove
				ifTrue: [ 
					matchSourceStart > 0
						ifTrue: [ 
							joins
								add:
									(self newMatchJoinSectionFrom: (matchSourceStart to: sourceLine) to: (matchDestinationStart to: destinationLine)).
							matchSourceStart := matchDestinationStart := 0 ].	
					sourceLine := sourceLine + 1.
					destinationLine := destinationLine + 1.
					sourceRunStart = 0 
						ifTrue: [ sourceRunStart := sourceLine ].
					sourceRunEnd := sourceLine.
					destinationRunStart = 0
						ifTrue: [ destinationRunStart := destinationLine ].
					destinationRunEnd := destinationLine.
					type := #removal].
			p key = #insert
				ifTrue: [ 
					matchSourceStart > 0
						ifTrue: [ 
							joins
								add:
									(self newMatchJoinSectionFrom: (matchSourceStart to: sourceLine) to: (matchDestinationStart to: destinationLine)).
							matchSourceStart := matchDestinationStart := 0 ].
	destinationLine := destinationLine + 1.
	copyLine := copyLine + 1.
	sourceRunStart > 0 
		ifTrue: [ 
			destinationRunStart = 0
				ifTrue: [ destinationRunStart := destinationLine ].
			copyRunStart = 0
				ifTrue: [ copyRunStart := copyLine ].
			destinationRunEnd = 0
				ifTrue: [ destinationRunEnd := destinationRunStart - 1 ].
			copyRunEnd = 0
				ifTrue: [ copyRunEnd := copyRunStart - 1 ].
			joins
				add: (self newJoinSectionFrom: (sourceRunStart to: sourceRunEnd) 
						to: (destinationRunStart to: destinationRunEnd)
						copyRange: (copyRunStart to: copyRunEnd )  
						withType: type
						flagRemove: (destinationRunEnd - destinationRunStart) - (sourceRunEnd - sourceRunStart) ).
							destinationRunStart := destinationRunEnd := sourceRunStart := sourceRunEnd := copyRunStart := copyRunEnd := 0].
					destinationRunStart = 0
						ifTrue: [ destinationRunStart := destinationLine ].
					destinationRunEnd := destinationLine.
					copyRunStart = 0
						ifTrue: [ copyRunStart := copyLine ].
					copyRunEnd := copyLine.
					 type := #addition] ].
	sourceLine := sourceLine + 1.
	destinationLine := destinationLine + 1.
	(destinationRunStart > 0 or: [ sourceRunStart > 0 ])
		ifTrue: [ 
			sourceRunStart = 0
				ifTrue: [ sourceRunStart := sourceLine ].
			destinationRunStart = 0
				ifTrue: [ destinationRunStart := destinationLine ].
			sourceRunEnd = 0
				ifTrue: [ sourceRunEnd := sourceRunStart - 1 ].
			destinationRunEnd = 0
				ifTrue: [ destinationRunEnd := destinationRunStart - 1 ].
			copyRunEnd = 0
				ifTrue: [ copyRunEnd := copyRunStart - 1 ].
			type = #addition ifTrue: [ copyLine := copyLine + 1.
				lineRemove := (destinationRunEnd - destinationRunStart) - (copyRunEnd - copyRunStart) ].
			copyRunStart = 0
				ifTrue: [ type = #removal ifFalse: [ copyRunStart := copyLine]
					ifTrue: [ copyRunStart := copyLine + 1]].
					type = #removal ifTrue: [
						lineRemove := (destinationRunEnd - destinationRunStart) - (sourceRunEnd - sourceRunStart)].
				joins add: (self newJoinSectionFrom: (sourceRunStart to: sourceRunEnd) 
					to: (destinationRunStart to: destinationRunEnd)
					copyRange: (copyRunStart to: copyRunEnd ) 
					withType: type
					flagRemove: lineRemove )].
	matchSourceStart > 0
		ifTrue: [ 
			joins
				add:
					(self newMatchJoinSectionFrom: (matchSourceStart to: sourceLine - 1) to: (matchDestinationStart to: destinationLine - 1)) ].
	^ joins
]

{ #category : 'instance creation' }
UnifiedDiffChangesMorph >> from: old to: new [
	"Set the old (src) and new (dst) text."
	self sourceTextModel setText: old.
	self destTextModel setText: new.
	
	self
		applyPrettyPrinter;
		calculateDifference;
		newStringOfDifference;
		calculateJoinMappings;
		calibrateScrollbar;
		applyHighlights;
		applyJoin;
		applyMap
]

{ #category : 'instance creation' }
UnifiedDiffChangesMorph >> from: old to: new contextClass: aClass [

	"Set the old (src) and new (dst) text."
	self contextClass: aClass.
	self sourceTextModel setText: old.
	self destTextModel setText: new.
	
	self
		applyPrettyPrinter;
		calculateDifference;
		newStringOfDifference;
		calculateJoinMappings;
		calibrateScrollbar;
		applyHighlights;
		applyJoin;
		applyMap
]

{ #category : 'initialization' }
UnifiedDiffChangesMorph >> initialize [
	"Initialize the receiver."

	super initialize.
	copyDstMorph := self newDstMorph .
]

{ #category : 'actions' }
UnifiedDiffChangesMorph >> joinSectionClass [
	"Answer the class to use for a new join section."

	^UnifiedDiffJoinSection
]

{ #category : 'actions' }
UnifiedDiffChangesMorph >> newCopyMorph [
	"Answer a new dst text morph."
	|copy|
	copy := RubScrolledTextModel new interactionModel: self.
	copy setText: destTextModel getText.
		^ copy newScrolledText
		vScrollbarShowNever;
		beNotWrapped;
		beReadOnly;
		yourself
]

{ #category : 'instance creation' }
UnifiedDiffChangesMorph >> newDstMorph [
	"Answer a new dst text morph."

	^ self destTextModel newScrolledText
		vScrollbarShowNever;
		"beForSmalltalkCode;"
		beNotWrapped;
		beReadOnly;
		yourself
]

{ #category : 'actions' }
UnifiedDiffChangesMorph >> newJoinSectionFrom: srcRange to: dstRange copyRange: copyRange withType: aType flagRemove: aNumber [ 
	"Answer a new join section."
	
	|sourceParagraphLines destinationParagraphLines copyParagraphLines sourceTopPixelPosition sourceBottomPixelPosition destinationTopPixelPosition destinationBottomPixelPosition type rectangleColor copyTopPixelPosition copyBottomPixelPosition|
	sourceParagraphLines := self srcMorph textMorph paragraph lines.
	destinationParagraphLines := self dstMorph textMorph paragraph lines.
	copyParagraphLines := copyDstMorph textMorph paragraph lines.
	type := #modification.
	sourceTopPixelPosition := srcRange first > sourceParagraphLines size
		ifTrue: [type := aType.
				sourceParagraphLines last bottom truncated - 1]
		ifFalse: [(sourceParagraphLines at: srcRange first) top truncated - 1].
	sourceBottomPixelPosition := srcRange size < 1
		ifTrue: [type := aType.
				 sourceTopPixelPosition + 3]
		ifFalse: [srcRange last > sourceParagraphLines size
				ifTrue: [sourceParagraphLines last bottom truncated + 3]
				ifFalse: [(sourceParagraphLines at: srcRange last) bottom truncated - 1]].
	destinationTopPixelPosition := dstRange first > destinationParagraphLines size
		ifTrue: [type := aType.
				destinationParagraphLines last bottom truncated - 1]
		ifFalse: [(destinationParagraphLines at: dstRange first) top truncated - 1].
	destinationBottomPixelPosition := dstRange size < 1
		ifTrue: [type := aType.
				destinationTopPixelPosition + 3]
		ifFalse: [dstRange last > destinationParagraphLines size
				ifTrue: [destinationParagraphLines last bottom truncated + 3]
				ifFalse: [(destinationParagraphLines at: dstRange last) bottom truncated - 1]].
	copyTopPixelPosition := copyRange first > destinationParagraphLines size
		ifTrue: [type := #removal.
				copyParagraphLines last bottom truncated - 1]
		ifFalse: [(copyParagraphLines at: copyRange first) top truncated - 1].
	copyBottomPixelPosition := copyRange size < 1
		ifTrue: [type := #removal.
				destinationTopPixelPosition + 3]
		ifFalse: [copyRange last > copyParagraphLines size
				ifTrue: [copyParagraphLines last bottom truncated + 3]
				ifFalse: [(copyParagraphLines at: copyRange last) bottom truncated - 1]].
	rectangleColor := self colorForType: type.
	^self newJoinSection
		type: type;
		srcColor: rectangleColor;
		dstColor: rectangleColor;
		srcLineRange: srcRange ;
		dstLineRange: dstRange;
		copyLineRange: copyRange;
		srcRange: (sourceTopPixelPosition to: sourceBottomPixelPosition);
		dstRange: (destinationTopPixelPosition to: destinationBottomPixelPosition);
		copyRange: (copyTopPixelPosition to: copyBottomPixelPosition );
		createHighlightsFrom: self srcMorph textMorph paragraph
		to: self dstMorph textMorph paragraph
		withCopy: copyDstMorph textMorph paragraph
		flagRemove: aNumber
]

{ #category : 'actions' }
UnifiedDiffChangesMorph >> newStringOfDifference [
	unifiedText := String streamContents: [:s | (self difference  collect: #value) asStringOn: s delimiter: String cr].
	copyDstMorph :=  self newCopyMorph.
	self destTextModel setText: unifiedText.
	
]
