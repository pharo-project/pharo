A GLMPane represents the "physical" building block of a browser. A pane is presented using a composite presentation (held in the presentations instance var).

It announces:
- GLMMatchingPresentationsChanged
- GLMPresentationsChanged

Instance Variables
	browser:		Browser
	lastActivePresentation:		Presentation
	name:		Symbol
	ports:		Collection of Ports
	presentations:		CompositePresentation