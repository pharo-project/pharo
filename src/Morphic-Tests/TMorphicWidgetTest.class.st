"
I test the theme access of widgets.
"
Class {
	#name : 'TMorphicWidgetTest',
	#superclass : 'TestCase',
	#category : 'Morphic-Tests-Widgets',
	#package : 'Morphic-Tests',
	#tag : 'Widgets'
}

{ #category : 'tests' }
TMorphicWidgetTest >> testCoreMorphsAreNotStyled [

	self deny: Morph new balloonHelpEnabled.
	self assert: BorderedMorph new borderWidth equals: 1.
	self assert: StringMorph new color equals: Color black.
	self deny: Morph new tabAmongFields
]

{ #category : 'tests' }
TMorphicWidgetTest >> testMorphicCoreDefinesNoBalloonStyle [

	| core |
	core := 'Morphic-Core' asPackage.
	#( #balloonColor #balloonColor: #balloonFont #balloonFont: ) do: [ :selector |
		self deny: (Morph >> selector) package equals: core ]
]

{ #category : 'tests' }
TMorphicWidgetTest >> testMorphicCoreDefinesNoThemeAccess [

	| core |
	core := 'Morphic-Core' asPackage.
	{
		(Morph >> #theme).
		(Morph >> #theme:).
		(Morph >> #basicTheme:).
		(Morph class >> #theme).
		(WorldState class >> #theme) } do: [ :method | self deny: method package equals: core ]
]

{ #category : 'tests' }
TMorphicWidgetTest >> testThemeComesFromOwner [

	| theme owner widget |
	theme := UITheme current class new.
	owner := Morph new.
	owner setProperty: #theme toValue: theme.
	widget := LabelMorph new.
	owner addMorph: widget.
	self assert: widget theme identicalTo: theme
]

{ #category : 'tests' }
TMorphicWidgetTest >> testThemeDefaultsToCurrent [

	self assert: LabelMorph new theme identicalTo: UITheme current
]

{ #category : 'tests' }
TMorphicWidgetTest >> testThemeSetOnWidget [

	| theme widget |
	theme := UITheme current class new.
	widget := LabelMorph new.
	widget basicTheme: theme.
	self assert: widget theme identicalTo: theme
]

{ #category : 'tests' }
TMorphicWidgetTest >> testWidgetsTakeTheThemeStyle [

	| theme |
	theme := UITheme current.
	self assert: LabelMorph new color equals: theme textColor.
	self assert: MenuItemMorph new color equals: theme textColor.
	self assert: LabelMorph new balloonHelpEnabled equals: theme settings balloonHelpEnabled
]
