Extension { #name : #UITheme }

{ #category : #'*Polymorph-Widgets-Themes' }
UITheme >> configureWatcherWindowLabelAreaFor: aWindow [
	"Configure the label area for the given Watcher window."

	|padding|
	padding := 0.
	aWindow labelArea
		addMorphBack: (Morph new extent: aWindow class borderWidth @ 0).
	aWindow hasCloseBox ifTrue: [aWindow addCloseBox. padding := padding + 1].
"	aWindow hasCollapseBox ifTrue: [aWindow addCollapseBox. padding := padding + 1].
	aWindow hasExpandBox ifTrue: [aWindow addExpandBox. padding := padding + 1].
	aWindow hasMenuBox ifTrue: [padding := padding - 1].
"	aWindow labelArea
		addMorphBack: (Morph new extent: aWindow class borderWidth @ 0; hResizing: #spaceFill).
	aWindow basicLabel ifNotNil: [:label | aWindow labelArea addMorphBack: label; hResizing: #shrinkWrap].
	aWindow labelArea
		addMorphBack: (Morph new extent: aWindow class borderWidth @ 0; hResizing: #spaceFill).
	padding > 0 ifTrue: [
		aWindow labelArea
			addMorphBack: (Morph new extent: (aWindow boxExtent x * padding) @ 0)].
"	aWindow hasMenuBox ifTrue: [aWindow addMenuControl]."
	aWindow labelArea
		addMorphBack: (Morph new extent: aWindow class borderWidth @ 0)
]

{ #category : #'*Polymorph-Widgets-Themes' }
UITheme >> watcherWindowActiveFillStyleFor: aWindow [

	^ SolidFillStyle color: (Color veryVeryLightGray alpha: 0.6)
]

{ #category : #'*Polymorph-Widgets-Themes' }
UITheme >> watcherWindowInactiveFillStyleFor: aWindow [

	^ SolidFillStyle color: (Color veryVeryLightGray alpha: 0.6)
]
