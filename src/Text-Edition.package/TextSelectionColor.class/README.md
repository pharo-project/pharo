I represent a text selection attribute that can be added to a text for a text emphazing (text color). I have a color block (colorBlock) which is evaluated in order to get the text color. 
I have three instances, one for the primary selection and the two others for the secondary selection and the find replace selection. 
PrimarySelection is used in TextEditor>>#storeSelectionInParagraph in order to emphasize the selection text if the selection text color setting is not nil.
SecondarySelection and FindReplaceSelection are manage by MultiNewParagraph if secondary selection text and find replace selection text colors are not nil.

Instance Variables
	colorBlock:		<Block>

colorBlock
	- The block which is evaluated in order to get the text color
