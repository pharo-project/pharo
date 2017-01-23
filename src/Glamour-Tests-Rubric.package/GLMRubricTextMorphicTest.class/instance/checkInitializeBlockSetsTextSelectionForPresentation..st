private
checkInitializeBlockSetsTextSelectionForPresentation: aSelector
	| browser textMorph |
	
	browser := GLMTabulator new.
	browser act: [ :aBrowser | aBrowser entity: '2 + 3 + 4' ] 
		icon: GLMUIThemeExtraIcons glamorousRefresh 
		entitled: 'Change text'.
	browser row: #main.
	browser transmit to: #main; andShow: [ :c |
		(c perform: aSelector)
			title: 'Code';
			initialize: [ :script |
				script selectionInterval: (1 to: 5) ] ].
	window := browser openOn: ('1 + 2 + 3').

	textMorph := self find: RubScrolledTextMorph in: window.
	self assert: textMorph getSelectionFromModel equals: (1 to: 5).
	self assert: textMorph selectionInterval equals: (1 to: 5).
	
	browser entity: '2 + 3 + 4'.
	self assert: textMorph getSelectionFromModel equals: (1 to: 5).
	self assert: textMorph selectionInterval equals: (1 to: 5).