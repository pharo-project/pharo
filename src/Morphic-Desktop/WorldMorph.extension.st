Extension { #name : 'WorldMorph' }

{ #category : '*Morphic-Desktop' }
WorldMorph >> contentsMenu: aMenu [
	"Build the menu used from PopUpContentsMenu:"

	| expanded collapsed |
	expanded := self windowsSatisfying: [ :w | w isCollapsed not ].
	collapsed := self windowsSatisfying: [ :w | w isCollapsed ].
	(expanded asSortedCollection: [ :w1 :w2 | w1 label caseInsensitiveLessOrEqual: w2 label ])
		do: [ :w |
			aMenu add: (self truncatedMenuLabelFor: w label) target: w selector: #activateAndForceLabelToShow.
			aMenu lastItem iconFormSet: (self iconFormSetNamed: w taskbarIconName).
			w model canDiscardEdits
				ifFalse: [ aMenu lastItem color: Color red ] ].
	aMenu addLine.
	(collapsed asSortedCollection: [ :w1 :w2 | w1 label caseInsensitiveLessOrEqual: w2 label ])
		do: [ :w |
			aMenu add: (self truncatedMenuLabelFor: w label) target: w selector: #collapseOrExpand.
			aMenu lastItem iconFormSet: (self iconFormSetNamed: w taskbarIconName).
			w model canDiscardEdits
				ifFalse: [ aMenu lastItem color: Color red ] ].
	aMenu addLine.
	((self submorphs reject: [ :sm | (expanded includes: sm) or: [ collapsed includes: sm ] ])
		asSortedCollection: [ :w1 :w2 | w1 class name caseInsensitiveLessOrEqual: w2 class name ])
		do: [ :w |
			aMenu add: (self truncatedMenuLabelFor: w class name) target: w selector: #comeToFront.
			aMenu lastItem iconFormSet: (self iconFormSetNamed: w taskbarIconName) ].

	^ aMenu
]

{ #category : '*Morphic-Desktop' }
WorldMorph >> contentsMenuTitle [
	^ 'World contents' translated
]

{ #category : '*Morphic-Desktop' }
WorldMorph >> discoveredWorldMenu [
	^ worldState discoveredWorldMenu
]

{ #category : '*Morphic-Desktop' }
WorldMorph >> truncatedMenuLabelFor: aWindowLabel [
	^ aWindowLabel truncateWithElipsisTo: 47
]

{ #category : '*Morphic-Desktop' }
WorldMorph >> worldMenu [
	^ worldState worldMenu
]
