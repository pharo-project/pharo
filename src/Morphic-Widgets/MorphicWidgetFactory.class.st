"
I build themed widgets and open themed dialogs: alerts, questions, text entry, and color, font, file and drop list choosers. I draw them with the current UI theme, through `TEasilyThemed`.

## Design Rationale

- Dialogs are a widget job. Code that needs one asks me, not the UI manager, so Morphic-Core does not depend on widgets.
- I take my widgets and dialogs from `TEasilyThemed` instead of redefining them.

## Usage

1. Code that needs a dialog creates me with `MorphicWidgetFactory new` and sends me one message: `SystemWindow>>setWindowColor` sends `chooseColor:`, `WorldState>>saveAs` sends `chooseForSaveFileReference:extensions:path:`, `RubTextEditor>>compareToClipboard` sends `longMessage:title:`.
2. `MorphicUIManager>>widgetFactory` creates me to answer the `UIManager` dialogs (`alert:`, `question:`, `textEntry:`…).

I hold no state: create one per request and drop it.
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
