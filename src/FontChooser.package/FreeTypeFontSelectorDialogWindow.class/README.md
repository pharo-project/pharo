I'm a dialog to select a font. 

Usage example:

|fontSelector|
fontSelector := FreeTypeFontSelectorDialogWindow new.
UITheme builder openModal: fontSelector.
fontSelector selectedFont inspect.