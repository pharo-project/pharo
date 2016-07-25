accessing
extent: aPoint 
	"Change the receiver's extent.
	optimized to not keep updating the (gradient) color!"
	
	|old|
	old := self extent.
	super extent: aPoint.
	self extent = old ifTrue: [^self].
	self updateColor