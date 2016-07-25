scrollbars
basicCreateArrowOfDirection: aSymbolDirection size: finalSizeInteger color: aColor 
	"PRIVATE - create an arrow with aSymbolDirectionDirection,  
	finalSizeInteger and aColor.
	aSymbolDirectionDirection = #top, #bottom. #left or #right .
	Try with:  
	(UITheme current
		basicCreateArrowOfDirection: #top 
		size: 32 
		color: Color lightGreen) asMorph openInHand. 
	Fixed to work with translucent colours."
	
	|form resizeFactor arrow resizedForm |
	resizeFactor := 4.
	form := Form
		extent: finalSizeInteger asPoint * resizeFactor
		depth: Display depth.
	form fillColor: Color transparent.
	arrow := (self createArrowOfDirection: aSymbolDirection in: form boundingBox)
		borderWidth: 1;
		borderColor: aColor lighter.
	self
		updateScrollBarButtonAspect: arrow
		color: (aColor alphaMixed: 0.7 with: Color black).
	arrow fullDrawOn: form getCanvas.
	resizedForm := form
		magnify: form boundingBox
		by: 1 / resizeFactor
		smoothing: 4.
	^resizedForm trimBordersOfColor: Color transparent