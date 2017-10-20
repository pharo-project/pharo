api
buildAdapterWithSpec
	"Build the widget using the spec name provided as argument"
	| adapter widget aSpecLayout |	
	aSpecLayout := (self retrieveSpec: self defaultSpecSelector).
	adapter := SpecInterpreter 
		private_interpretASpec: aSpecLayout
		model: self
		selector: self defaultSpecSelector.
	widget := adapter widget.
	self setExtentAndBindingTo: widget. 
	^ adapter