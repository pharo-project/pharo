This example shows how to use shout for method editing. Try it with:

SHMethodEditingMode new open

For method editing, you need to provide a class to the styler. When a class is given, the styler is automatically turned into a method editing mode.
The styler is then able to style the code according to the class bindings (instance variables names, shared pool...)
See #shoutAboutToStyle. 
  - First, the class is given to the styler by sending #classOrMetaClass: to the view with the class as argument.
  - Finally, in this example, #shoutAboutToStyle returns true because the code is always to be styled.

Instance Variables
	className:		<String>
	code:		<String>

className
	- The class name for the styler

code
	- the code which is entered into the editor  
