private
buildWithSpecLayout: aSpecLayout
	"Build the widget using the spec name provided as argument"
	| adapter widget|
	adapter := self adapterFrom: aSpecLayout model: self.
	widget := adapter asWidget.
	self announce: (WidgetBuilt model: self widget: widget).
	^ widget