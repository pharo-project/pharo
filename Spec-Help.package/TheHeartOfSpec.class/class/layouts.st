pages
layouts
	^HelpTopic
		title: 'The layout method'
		contents: 'This method specifies the layout of the different widgets in the UI. It also specifies how a widget reacts when the window is resized. For the same UI multiple layouts can be described, and when the UI is built a specific layout to use can be specified. If no such specific layout is given, the following lookup mechanism will be used to obtain the layout method:

1. Search on class side, throughout the whole class hierarchy, for a method with the pragma <spec: #default>.
2. If multiple such methods exist, the first one found is used.
3. If none such methods exist and if there is exactly one method with the pragma <spec>, this method is used.
4. No layout method is found, an error is raised.

This method is on class side because it returns a value that usually is the same for all the instances. Put differently, usually all the instances of the same user interface have the same layout and hence this can be considered as being a class-side accessor for a class variable. Note that the lookup for the spec method to use starts on instance side, which allows a UI to have a more specific layout depending on the state of the instance.

In its simplest form, this method could be:

	^ SpecLayout composed
			add: #theList;
			yourself


The symbol theList refers to an instance side method returning a widget. This is because instance variables are private, so the layout class needs to use an accessor to obtain it when building the UI. Note that by default, a widget will take all the space available in its container.
This method is not restricted to laying out sub widgets. It can also refer to sub widgets contained in sub widgets, i.e. when reusing an existing UI, specify a new layout for the sub widgets that comprise this UI. To do this, instead of giving a symbol, an array with 2 symbols must be given. 
The first symbol identifies the UI being reused and the second the sub widget within this UI whose new layout position is being specified. 
As said above, multiple layouts can be described for the same user interface. In order to retrieve the correct method to apply, these methods need to be flagged with a pragma. The pragma can be either <spec: default> for the layout to use by default, or <spec> for the other layouts.

Specifying this method is mandatory, as without it the UI would show no widgets to the user.'
