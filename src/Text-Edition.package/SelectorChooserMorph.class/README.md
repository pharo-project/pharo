A SelectorChooserMorph is an IdentifierChooserMorph specialized to allow a selector query from a selector prefix and for a TextMorph. 
When the prefix starts with an uppercase, then, only globals names and class names that begins with the prefix are shown.
Otherwise, all systems symbols that begins with the prefix are presented (the string comparison is not case sensitive).
It takes the keyboard focus when a navigation key (up, down, left or right key) is first pressed in the TextMorph from which it has been opened.

Instance Variables
	prefix: <String>
	requestorPos <Point>
	
prefix
	- The prefix which has been used to compute the labels list.

requestorPos
	- private, used to be able to automatically close the menu if the position of the requestor is changing