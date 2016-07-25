api
buildWithSpecLayout: aSpecLayout
	"Build the widget using the spec name provided as argument"
	| widget adapter|
	adapter := self adapterFrom: aSpecLayout model: self.
	widget := adapter asWidget.
	self setExtentAndBindingTo: widget.
	^ widget