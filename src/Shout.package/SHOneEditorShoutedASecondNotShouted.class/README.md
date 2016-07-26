In this example, the view has two text editors. 
Try it with:

SHOneEditorShoutedASecondNotShouted  new open.

The top view is supposed to be used for code editing and the other for comment editing.
Only the code editor view has to be highligthed whereas the comment editor is never highligthed. In this case, the model must implement a #shoutAboutToStyle: method which returns true. When the view is built, the comment pane is setup to forbid code styling by sending #styled: to it with false as argument. See the #open method and the #styled: sent.

Instance Variables:
	code	<Text>
	comment	<Text>