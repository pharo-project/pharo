"
I build themed widgets and open themed dialogs: alerts, questions, text entry, color, font and file choosers.
I take the current theme. Use me where code needs a dialog; the UI manager delegates its dialogs to me.
"
Class {
	#name : 'MorphicWidgetFactory',
	#superclass : 'Object',
	#traits : 'TEasilyThemed',
	#classTraits : 'TEasilyThemed classTrait',
	#category : 'Morphic-Widgets-Theme',
	#package : 'Morphic-Widgets',
	#tag : 'Theme'
}
