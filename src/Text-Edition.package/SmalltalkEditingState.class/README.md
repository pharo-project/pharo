See EditingState comments.
SmalltalkEditingState is made to manage data for Smalltalk code editing sessions (such as a selector chooser).

Instance Variables:
   selectorChooser <SelectorChooserMorph>

- selectorChooser
The basic selector chooser which is popup to fetch available selectors or class names begining with a prefix. The prefix is token from the current caret location (see SelectorChooserMorph comments).
