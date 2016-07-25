I'm a example that shows how to have a simple editor in which text is highlighted or not depending on the editor model state.  Try it with:

SHStylingOrNotDependingOnModelState new open.

Thus, depending on the model state, the view should be styled or not styled. This is achieve by implementing a #shoutAboutToStyle: method in the model (here, an instance of myself). #shoutAboutToStyle: returns true if the view which is passed as argument must be styled. In this excample, if the value of the 'state' instance variable is #code, then the text is hilighted, else it is not.

Instance Variables:
	code	<Text>
	codeEditing    <Boolean>