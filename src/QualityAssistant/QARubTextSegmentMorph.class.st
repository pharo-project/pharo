"
I am a slight modification of the rubric text segment morph which creates custom menu items for displaying critiques
"
Class {
	#name : #QARubTextSegmentMorph,
	#superclass : #RubTextSegmentMorph,
	#instVars : [
		'entity',
		'critique'
	],
	#category : 'QualityAssistant'
}

{ #category : #actions }
QARubTextSegmentMorph >> addItemToMenu: aMenu [
	| listItem |
	listItem := QARubMenuItemMorph new
		layoutPolicy: TableLayout new;
		listDirection: #leftToRight;
		cellPositioning: #center;
		hResizing: #spaceFill;
		vResizing: #shrinkWrap;
		cellInset: 5;
		color: Color transparent;
		"layoutInset: (Margin left: 0 top: 0 right: 10 bottom: 0);"
			yourself.
	listItem addMorphBack: (ImageMorph withForm: critique icon).
	listItem addMorphBack: critique title asStringMorph.
	critique actions
		do: [ :action | 
			listItem
				addMorphBack:
					(IconicButton new
						target: action;
						labelGraphic: action icon;
						actionSelector: #actOnCritic:ofEntity:;
						arguments:
							{critique.
							entity};
						color: Color transparent;
						helpText: action description;
						extent: 16 @ 16;
						borderWidth: 0;
						yourself) ].
	critique isCritique
		ifTrue: [ listItem
				addMorphBack:
					(Morph new
						color: Color transparent;
						hResizing: #spaceFill;
						height: 5;
						yourself).
			listItem addMorphBack: (QAFeedbackMorph new critique: critique) ].
	^ aMenu addMenuItem: listItem
]

{ #category : #accessing }
QARubTextSegmentMorph >> critique [
	^ critique
]

{ #category : #accessing }
QARubTextSegmentMorph >> critique: anObject [
	critique := anObject
]

{ #category : #accessing }
QARubTextSegmentMorph >> entity [
	^ entity
]

{ #category : #accessing }
QARubTextSegmentMorph >> entity: anObject [
	entity := anObject
]
