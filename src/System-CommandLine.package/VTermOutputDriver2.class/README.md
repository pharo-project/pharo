I convert high-level terminal commands  to characters sequences understood by the terminal.
I use a term info backend to communicate with the terminal. The default term info implementation is TermInfoCharacter.

I have two different style, the current styles (with any modifications the user made on it) and the installedStyle which is the style of the terminal.

I give access to the styles I'm not implementing with the method set
(For example self set:'1m'  will make the font bold)
I use a SharedPool named VTermOutputStyles
Example of usage:
| out |
	out := VTermOutputDriver2 stdout.
	out 
	tab;
	<< 'normal text with a tab';
	newLine;
	redFont;
	blueBackground: 'red text with blue background';
	newLine;
	bold;
	<< 'red bold text';
	bold;
	underline;
	newLine;
	<< 'underlined red bold text';
	insertLines:2;
	close
