I provide the capability to open a floating editor to edit something on the fly.
The user is responsible for deciding on how to open the editor. See #exampleEditableStringMorph class side as an example on how to do it with #on:send:to.

One opened:
- if the escape character is pressed or if the keyboard focus is lost or in case of a mouse down anywhere else than inside the editor then the editing is aborted.
- the Enter key or Cmd+s save the contents in the Morph, then an announcement of class RubMorphEdited is sent so that the user can take desired action (checking the input, definitively accept it or reject it).

see #exampleEditableStringMorph class side 

Internal Representation and Key Implementation Points.

Instance Variables
	announcer:		<Announcer>
	editor:		<RubScrolledTextMorph>
	acceptOnCR:	<Boolean>
	initialContents: <String>
	font: <LogicalFont>
	customizeValuable: <Valuable>

Implementation notes
