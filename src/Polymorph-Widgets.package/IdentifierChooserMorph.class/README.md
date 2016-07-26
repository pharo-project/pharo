An IdentifierChooserMorph is a menu builder which takes a list of labels as input and build/popup  a menu for them. The morph could be made of one menu in one column for all labels of of several menus in a scrollabe row. The action which is performed when a menu item is selected is also parametrized (see examples below).
The morph can take the keyboard focus and then, up, down, left and right arrows can be used to choose a menu item.
It is the responsibility of the user of this morph to decide when and how the keyboard focus is token.
The design is widely inpired from PopupChoiceDialogWindow.

example 1
A very simple example with three label. The nil value will be represented as a menu line in the resulting morph.
(IdentifierChooserMorph 
		labels: {'aaaaaa'. 'bbbbbbb'. nil. 'cccccccc'}
		chooseBlock: [ :chosen | UIManager default inform: (chosen, (' has been chosen' translated))])
			open
			
example 2
The same except that a color is specified		
(IdentifierChooserMorph 
		labels: {'aaaaaa'. 'bbbbbbb'. nil. 'cccccccc'}
		chooseBlock: [ :chosen | UIManager default inform: (chosen, (' has been chosen' translated))])
			baseColor: Color white;
			open

example 3
Allows the presentation of one menu (one column) vith two fixed labels followed by the list of all classes.
(IdentifierChooserMorph 
		labels: ({'aaaaaa'. 'bbbbbbb'}, { nil }, (Object allSubclasses collect: [:c | c theNonMetaClass  name]) asSet asArray sort)
		chooseBlock: [ :chosen | (Smalltalk globals at: chosen asSymbol) ifNotNil: [:c | c browse] ]) 
			oneMenuOfWidth: 300;
			baseColor: Color white;
			open

Instance Variables
	baseColor:		<Color>
	choiceMenus:		<Array>
	choicesMorph:		<AlignmentMorph>
	chooseBlock:		<Block>
	labels:		<Array>
	maxLines:		<Integer>
	requestor:		<Morph>
	scrollPaneWidth:		<Integer>

baseColor
	- The color used for the menu items and the receiver

choiceMenus
	- The array of EmbeddedMenuMorph

choicesMorph
	- The AlignmentMorph which contains all menus

chooseBlock
	- A block with one argument which is evaluated when a menu item is selected. The argument takes the chosen label as argument

labels
	- The array of labels 

maxLines
	- If not nil, gives the maximum number of lines for one menu

requestor
	- if not nil, a Morph from which the receiver is built

scrollPaneWidth
	- The maximum width of the scrollPane, this contraints the width of the receiver.
