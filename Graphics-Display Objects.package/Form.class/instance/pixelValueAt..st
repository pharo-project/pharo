pixel access
pixelValueAt: aPoint 
	"Return the raw pixel value at the given point. This pixel value depends on the receiver's depth. Typical clients use colorAt: to get a Color.  "

	^ (BitBlt bitPeekerFromForm: self) pixelAt: aPoint
