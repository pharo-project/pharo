StringMorph is a "lightweight" Morph to display a String. It supports only a single font, color, and emphasis combination. For multiple text styles, use TextMorph.

Structure:
instance var    	Type              Description 
font 			StrikeFont 		(normally nil; then the accessor #font gives back TextStyle 
				or nil			defaultFont) 
emphasis 		SmallInteger	bitmask determining character attributes (underline, bold, 								italics, narrow, struckout) 
contents 		String 			The text that will be displayed. 
hasFocus 		Boolean 		Do I have the keyboard focus or not? 

A StringMorph may also be used like a SimpleButtonMorph to do an action when clicked. Use the menu 'extras' / 'add mouseUpAction'.

The following propery will be defined:
aStringMorph valueOfProperty: #mouseUpCodeToRun