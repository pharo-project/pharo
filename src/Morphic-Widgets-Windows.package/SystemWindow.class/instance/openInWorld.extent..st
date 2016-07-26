open/close
openInWorld: aWorld extent: extent
	"This msg and its callees result in the window being activeOnlyOnTop"
	self position: (RealEstateAgent initialFrameFor: self initialExtent: extent world: aWorld) topLeft; extent: extent.
	^self openAsIsIn: aWorld