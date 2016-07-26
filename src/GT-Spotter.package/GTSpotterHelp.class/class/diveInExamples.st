documentation
diveInExamples
	^ HelpTopic 
		title: 'Dive-in Examples'
		contents: 
'Spotter allows you to navigate and search through nested objects.

Let''s see an example.

Suppose we want to see all Spotter extensions from the Behavior class. We know that an extension is marked with the #spotterOrder: pragma. So, we first look for the pragma:
	spotterOrder: #pragma
	
We find our pragma. Then we dive-in it through Meta+ArrowRight (or clicking on the arrow button). This gets us inside a PragmaType object and provides several search possibilities relevant for only this object. Note how the breadbrumb from on top of the search input box shows us two steps: Spotter and spotterOrder:. The first category give us the concrete usages of the pragma. As we are interested in only Behavior, we type:
	Behavior

We then ensure that the preview is open (Meta+p), and we can browse the methods from Behavior that are the #spotterOrder: pragma.

The dive-in activity is useful when the search involves multiple steps.'