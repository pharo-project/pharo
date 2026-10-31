"
I am the branch selector of a row in the Quick Project Loader: the branch that will be loaded, as plain text with a caret. I have no box: the caret says that I open a list, and the mouse over me shows my area. All rows start me at the same place, so that branches line up, and I am as wide as my text, so that the caret stays next to it.
"
Class {
	#name : 'QuickProjectBranchChipMorph',
	#superclass : 'QuickProjectIconButtonMorph',
	#instVars : [
		'label'
	],
	#category : 'QuickProjectLoader-UI',
	#package : 'QuickProjectLoader',
	#tag : 'UI'
}

{ #category : 'accessing' }
QuickProjectBranchChipMorph class >> font [

	| font |
	font := StandardFonts defaultFont.
	^ font copy pointSize: (font pointSize - 1 max: 8)
]

{ #category : 'instance creation' }
QuickProjectBranchChipMorph class >> label: aString action: aBlock [

	^ (self
		icon: (self iconNamed: #branch)
		help: 'Choose the branch to load'
		action: aBlock)
		label: aString;
		yourself
]

{ #category : 'accessing' }
QuickProjectBranchChipMorph class >> chipWidth [

	^ 92
]

{ #category : 'drawing' }
QuickProjectBranchChipMorph >> drawCaretOn: aCanvas [
	"A caret made of shrinking lines, so that it depends on no font."

	| left top |
	left := self right - 11.
	top := self center y - 1.
	0 to: 2 do: [ :i |
		aCanvas
			fillRectangle: ((left + i) @ (top + i) extent: (5 - (2 * i)) @ 1)
			color: self tintColor ]
]

{ #category : 'drawing' }
QuickProjectBranchChipMorph >> drawOn: aCanvas [

	| font textLeft |
	hovering ifTrue: [ aCanvas fillRectangle: self bounds color: self theme lightBackgroundColor ].
	aCanvas
		translucentImage: icon
		at: (self left + 3) @ (self center y - (icon height // 2)).
	font := self class font.
	textLeft := self left + icon width + 5.
	aCanvas
		drawString: self fittedLabel
		in: (textLeft @ (self center y - (font height // 2)) corner: self right @ self bottom)
		font: font
		color: self tintColor.
	self drawCaretOn: aCanvas
]

{ #category : 'accessing' }
QuickProjectBranchChipMorph >> fittedLabel [

	^ QuickProjectRowMorph
		fit: label
		font: self class font
		width: self class chipWidth - icon width - 19
]

{ #category : 'initialization' }
QuickProjectBranchChipMorph >> initialize [

	super initialize.
	label := ''.
	self extent: self class chipWidth @ 22
]

{ #category : 'accessing' }
QuickProjectBranchChipMorph >> label [

	^ label
]

{ #category : 'accessing' }
QuickProjectBranchChipMorph >> label: aString [

	label := aString.
	self width: (self class font widthOfString: self fittedLabel) + icon width + 19
]

{ #category : 'drawing' }
QuickProjectBranchChipMorph >> tintColor [
	"Text, icon and caret follow the mode, so that the only accent of the panel is the Load action."

	^ self theme textColor alpha: 0.75
]
