Tells a piece of text to be a certain way.

Select text, press Command-6, choose a attribute.  If selected text is of the form 
	Hi There<Smalltalk beep>
the part in angle brackets is saved for action, and the Hi There appears in the paragraph.  If selection has no angle brackets, use the whole thing as both the text and the action.

TextDoIt  --  eval as a Smalltalk expression (the part in angle brackets)

TextLink -- Show a method, class comment, class hierarchy, or class defintion.
	<Point extent:>, <Point Comment>, <Point Hierarchy>, or <Point Defintion> are what you type.

TextURL -- Show the web page. <www.disney.com>

These attributes of text need to be stored on the disk in a regular file-out.  It is done in this form: 	Hi There   
	in the text, and a Run containing   dSmalltalk beep;;
	Click here to see the extent:   
	in the text, and a Run containing   method LPoint extent:;
See RunArray class scanFrom: where decoding is done.
