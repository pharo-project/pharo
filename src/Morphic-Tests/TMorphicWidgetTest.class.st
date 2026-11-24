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
