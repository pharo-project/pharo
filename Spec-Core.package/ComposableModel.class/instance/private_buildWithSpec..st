private
private_buildWithSpec: aSpec
	"Build the widget using the spec name provided as argument"
	| adapter |
	adapter := self privateAdapterFromModel: self withSpec: aSpec.
	self setExtentAndBindingTo: adapter widget.	
	^ adapter