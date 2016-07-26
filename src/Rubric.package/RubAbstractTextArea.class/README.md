I am a re-implementation of TextMorph. I'm intended as a temporary solution. Soon, I will be replaced by, or integrated with, TxText, a beautiful new text toolkit.

About the contextual menu
By default, the menu is given by the editingMode.
The model can implement a #menu method to impose a specific menu.
To change the way the menu is looked-up, one can also change the getMenuPolicy.
This menu retrieving algorithm is implemented by a dedicated objet, hold by the RubAbstractTextArea>>#getMenuPolicy instance variable. This dedicated object implements a #lookupMenu method for this.
By default, it is the textArea itself (see RubAbstractTextArea>>defaultGetMenuPolicy). 
The policy can be changed with RubAbstractTextArea>>#getMenuPolicy:, by passing whatever object that answer to #lookupMenu.

Instance Variables
	editingMode:		<Object>
	editingState:		<Object>
	editor:		<Object>
	hasFocus:		<Object>
	holder:		<Object>
	margins:		<Object>
	menuAllowed:		<Object>
	model:		<Object>
	paragraph:		<Object>
	readOnly:		<Object>
	scrollPivot:		<Object>
	text:		<Object>
	textColor:		<Object>
	textStyle:		<Object>
	wrapped:		<Object>

editingMode
	- xxxxx

editingState
	- xxxxx

editor
	- xxxxx

hasFocus
	- xxxxx

holder
	- xxxxx

margins
	- xxxxx

menuAllowed
	- xxxxx

model
	- xxxxx

paragraph
	- xxxxx

readOnly
	- xxxxx

scrollPivot
	- xxxxx

text
	- xxxxx

textColor
	- xxxxx

textStyle
	- xxxxx

wrapped
	- xxxxx


