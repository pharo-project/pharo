I'm a pluggable togglable button. 
I extend a pluggable button in a very simple way:  I keep two states, PRESSED, and UNPRESSED (in fact, is just a boolean).

I override a couple of methods, like #getModelState because in my context it does not have sense. 

See PluggableToggleButtonMorphExample to have some usage hints. 

Example:
========
(This is very simple and probably you want to use as is shown in the examples)

PluggableToggleButtonMorph new 
	icon: Smalltalk ui icons smallOkIcon;
	label: 'Test';
	actionBlock: [ :pressed | self inform: 'Status: ', (pressed asString) ];
	openInWindow
	
