"
I convert IconStylerDecoration instances into Rubric text segments.

Used for backward compatibility with Calypso Morphic tools.
"
Class {
	#name : 'RubricIconStylerRenderer',
	#superclass : 'IconStylerRenderer',
	#category : 'Reflectivity-Tools-Styler',
	#package : 'Reflectivity-Tools',
	#tag : 'Styler'
}

{ #category : 'rendering' }
RubricIconStylerRenderer >> addDecoration: aDecoration [
	"Backward compat with Calypso — remove with Calypso migration to Spec."

	| conf |
	conf := RubConfigurationChange new.
	conf configurationBlock: [ :text |
			| segment |
			segment := (aDecoration isUnderline
				            ifTrue: [ RubUnderlinedSegmentMorph ]
				            ifFalse: [ RubTextSegmentMorph ]) from: aDecoration interval first to: aDecoration interval last.
			text addSegment: segment.
			segment label: aDecoration title.
			segment icon: aDecoration icon.
			segment iconBlock: aDecoration iconBlock.
			segment color: aDecoration highlightColor.
			aDecoration isUnderline ifTrue: [ segment underlineColor: aDecoration underlineColor ] ].
	textModel announce: conf
]

{ #category : 'rendering' }
RubricIconStylerRenderer >> clearDecorations [
	"Segments are cleared externally by Calypso callers."

	
]
