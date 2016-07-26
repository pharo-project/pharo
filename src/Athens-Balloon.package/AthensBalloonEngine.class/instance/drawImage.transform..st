drawing
drawImage: aForm transform: m 
	| orig dir normal tr corner rect |

	rect := aForm boundingBox.
	orig := 0@0. 
	dir := aForm width @ 0.
	normal := 0 @ aForm height.
	
	tr := edgeTransform.
	edgeTransform := m.

	self prepareForRendering.
	self registerFormFill: aForm
		origin: orig
		direction: dir
		normal: normal.
	self fillRectangle: rect.

	edgeTransform := tr.
