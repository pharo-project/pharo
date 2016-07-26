displaying-generic
displayOn: aDisplayMedium at: aDisplayPoint clippingBox: clipRectangle rule: ruleInteger fillColor: aForm
	"This is the basic display primitive for graphic display objects. Display 
	the receiver located at aDisplayPoint with rule, ruleInteger, and mask, 
	aForm. Information to be displayed must be confined to the area that 
	intersects with clipRectangle."

	self subclassResponsibility