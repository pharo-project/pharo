pages
about
	^HelpTopic
		title: 'About'
		contents: 
'The Refactoring Engine was originally developed by Don Roberts and John Brant for VisualWorks. It was ported to squeak and pharo with extensions by Lukas Renggli and other contributors.

One goal of this framework was to easily include the code refactoring into the standard development work flow. The refactoring operations should help to transform and restructure the source code, without to much manual intervention. And without the need to retest every single change.

Therefore some basic primitive refactoring are provided, in a way that more complex operations can be constructed by compositing the primitive ones. 

A refactoring tool should provide an environment for viewing the code, the refactoring transformation and the result. Originally a standalone tool, later the support for refactoring operations were directly integrated into the standard Smalltalk System Browser.

The Tool, or a browser with refactoring support or the whole framework is often just known as the ''Refactoring Browser''. 

That''s why all of the classes of this framework start with the prefix ''RB''.
'