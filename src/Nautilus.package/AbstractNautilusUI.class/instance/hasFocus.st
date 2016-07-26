displaying
hasFocus

	^ window hasKeyboardFocus or: 
		[ self packageWidget hasFocus or:
		[ self classWidget hasFocus or: 
		[ self categoryWidget hasFocus or:
		[ self methodWidget hasFocus "or:
		[ sourceTextArea hasKeyboardFocus ]"]]]]