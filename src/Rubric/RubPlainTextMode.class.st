"
I represent the mode to use to edit plain text (in menus, help browser etc)
"
Class {
	#name : #RubPlainTextMode,
	#superclass : #RubEditingMode,
	#category : 'Rubric-Editing-Core'
}

{ #category : #menu }
RubPlainTextMode class >> editingModeMenuOn: aBuilder [
	"Specify the menu used to select an editing mode"

	<contextMenu>
	<RubEditingModeMenu>
	(aBuilder item: self label translated)
		action: [:model | model textArea editingMode: self new ]
]

{ #category : #menu }
RubPlainTextMode class >> fontMenuOn: aBuilder [
	"Specify the menu used when writing text. Try it with:
	(PragmaMenuBuilder 
		pragmaKeyword: 'RubTextEditorShiftedMenu'
		model: nil) menu popUpInWorld"

	(aBuilder item: #'fontItems')
		label: 'Font...';
		withSeparatorAfter;
		with: [ 
					(aBuilder item: #'Set font...' translated)
						keyText: 'k';
						selector: #offerFontMenu.
					(aBuilder item: #'Set alignment...' translated)
						selector: #chooseAlignment;
						withSeparatorAfter ]
]

{ #category : #accessing }
RubPlainTextMode class >> label [
	^ 'Plain text'
]

{ #category : #menu }
RubPlainTextMode class >> menuKeyword [
	^ 'RubPlainTextMenu'
]

{ #category : #menu }
RubPlainTextMode class >> menuOn: aBuilder [
	"Specify the menu used when writing text. Try it with:
	(PragmaMenuBuilder 
		pragmaKeyword: 'RubPlainTextMenu'
		model: RubEditingArea new) menu popUpInWorld" 

	<contextMenu>
	<RubPlainTextMenu>
	
	(aBuilder item: #'Find...' translated)
		keyText: 'f';
		selector: #find;
		iconName: #smallFindIcon.
	(aBuilder item: #'Find again' translated)
		keyText: 'g';
		selector: #findAgain;
		iconName: #smallFindIcon.
	(aBuilder item: #'Set search string' translated)
		keyText: 'h';
		selector: #setSearchString.
	aBuilder model canChangeText
		ifTrue: [ 
			(aBuilder item: #'Do again' translated)
				keyText: 'j';
				selector: #again;
				iconName: #smallRedoIcon.
			(aBuilder item: #Undo translated)
				keyText: 'z';
				selector: #undo;
				iconName: #smallUndoIcon;
				withSeparatorAfter.
			self fontMenuOn: aBuilder ].
	(aBuilder item: #Copy translated)
		keyText: 'c';
		selector: #copySelection;
		iconName: #smallCopyIcon.
	aBuilder model canChangeText
		ifTrue: [ 
			(aBuilder item: #Cut translated)
				keyText: 'x';
				selector: #cut;
				iconName: #smallCutIcon.
			(aBuilder item: #Paste translated)
				keyText: 'v';
				selector: #paste;
				iconName: #smallPasteIcon.
			(aBuilder item: #'Paste...' translated)
				selector: #pasteRecent;
				iconName: #smallCopyIcon;
				withSeparatorAfter.
			(aBuilder model model notNil and: [ aBuilder model acceptAllowed ])
				ifTrue: [ 
					(aBuilder item: #Accept translated)
						keyText: 's';
						selector: #accept;
						iconName: #smallOkIcon.
					(aBuilder item: #Cancel translated)
						keyText: 'l';
						selector: #cancel;
						iconName: #smallCancelIcon ] ]
]

{ #category : #'text area font' }
RubPlainTextMode >> beAllFont: aFont [
	self textArea font: aFont
]

{ #category : #'text area emphasis' }
RubPlainTextMode >> bold [
	
	self changeEmphasis: #bold
]

{ #category : #'text area alignment' }
RubPlainTextMode >> centered [ 
	self changeAlignment: #centered
]

{ #category : #'text area alignment' }
RubPlainTextMode >> changeAlignment: aSymbol [
	"Change the alignment of the receiver. Alignment can be #leftFlush #centered #rightFlush #justified "
	self changeAttribute: (TextAlignment perform: aSymbol)
]

{ #category : #'text area attribute' }
RubPlainTextMode >> changeAttribute: anAttribute [
	"Change a TextAttribute  of the receiver. Emphasis, alignment..."

	self textArea
		beEditableWhile: [ 
			self textArea
				restoreSelectionIndexesAfter: [ 
					self textArea selectAll.
					self editor applyAttribute: anAttribute ] ].
	self textArea changed
]

{ #category : #'text area emphasis' }
RubPlainTextMode >> changeEmphasis: aSymbol [
	"Change the alignment of the receiver. Alignment can be #normal, #bold,  #italic, #narrow, #underlined, or #struckOut"

	self changeAttribute: (TextEmphasis perform: aSymbol)

]

{ #category : #'text area font' }
RubPlainTextMode >> fontName: fontName pointSize: pointSize [
	| newTextStyle |
	(LogicalFontManager current allFamilies detect: [ :f | f familyName = fontName ] ifNone: [  ])
		ifNotNil: [ :fam | 
			| style font |
			style := fam members detect: [ :m | m styleName = 'Regular' ].
			font := style asLogicalFontOfPointSize: pointSize.
			newTextStyle := font textStyle ]
		ifNil: [ newTextStyle := ((TextStyle named: fontName asSymbol) ifNil: [ TextStyle default ]) copy ].
	self textArea privateTextStyle: newTextStyle.
	self textArea addAttribute: (TextFontChange fontNumber: (newTextStyle fontIndexOfPointSize: pointSize))
]

{ #category : #'text area font' }
RubPlainTextMode >> fontName: fontName size: pointSize [
	| newTextStyle |
	(LogicalFontManager current allFamilies detect: [ :f | f familyName = fontName ] ifNone: [  ])
		ifNotNil: [ :fam | 
			| style font |
			style := fam members detect: [ :m | m styleName = 'Regular' ].
			font := style asLogicalFontOfPointSize: pointSize.
			newTextStyle := font textStyle ]
		ifNil: [ newTextStyle := ((TextStyle named: fontName asSymbol) ifNil: [ TextStyle default ]) copy ].
	self textArea privateTextStyle: newTextStyle.
	self addAttribute: (TextFontChange fontNumber: (newTextStyle fontIndexOfSize: pointSize))
]

{ #category : #'text area font' }
RubPlainTextMode >> fontPointSize: pointSize [
	self textArea addAttribute: (TextFontChange fontNumber: (self textStyle fontIndexOfPointSize: pointSize))
]

{ #category : #'text area emphasis' }
RubPlainTextMode >> italic [
	
	self changeEmphasis: #italic
]

{ #category : #'text area alignment' }
RubPlainTextMode >> justified [ 
	self changeAlignment: #justified
]

{ #category : #'text area alignment' }
RubPlainTextMode >> leftFlush [ 

	self changeAlignment: #leftFlush

]

{ #category : #'text area emphasis' }
RubPlainTextMode >> narrow [
	
	self changeEmphasis: #narrow
]

{ #category : #'text area emphasis' }
RubPlainTextMode >> normal [
	
	self changeEmphasis: #normal
]

{ #category : #'text area alignment' }
RubPlainTextMode >> rightFlush [ 
	
	self changeAlignment: #rightFlush

]

{ #category : #'text area emphasis' }
RubPlainTextMode >> struckOut [
	
	self changeEmphasis: #struckOut
]

{ #category : #'text area emphasis' }
RubPlainTextMode >> underlined [
	
	self changeEmphasis: #underlined
]
