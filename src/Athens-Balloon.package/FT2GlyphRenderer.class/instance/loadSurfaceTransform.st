private
loadSurfaceTransform
	| m mt org xaxis yaxis sum xmin xmax ymin ymax formW formH fix face bbox |
	
	face := font face.

	face 
		setPixelWidth: pixelSize height: pixelSize.
	
	bbox := face bbox.
	m := surface pathTransform copy transposed.

	org := m transform: 0@0.
	xaxis := (m transform: bbox right - bbox left * pixelSize / face unitsPerEm @ 0) - org.
	yaxis := (m transform: 0 @ (bbox bottom - bbox top * pixelSize / face unitsPerEm)) - org.
	sum := xaxis + yaxis.
	
	xmin := 0 min: ((xaxis x min: yaxis x) min: sum x).
	ymin := 0 min: ((xaxis y min: yaxis y) min: sum y).
	xmax := 0 max: ((xaxis x max: yaxis x) max: sum x).
	ymax := 0 max: ((xaxis y max: yaxis y) max: sum y).

	formW := (xmax - xmin) ceiling + 1.
	formH := (ymax - ymin) ceiling  + 1.

" freetype 'zero' points to bottom-left point on form" 

	"calculate bitmap translation relative to top-left corner of glyph"
	bitmapX := (yaxis x negated + xmin * 64.0) rounded.
	bitmapY := (xaxis y negated + ymin * 64.0) rounded.

	descend := 0 @  ((face bbox height -  face bbox bottom  * pixelSize / face unitsPerEm) +1).
	descend := (m transform: descend) - org.
	descend := descend + (xmin negated@(ymin negated)).
	descend := (descend * 64 ) rounded.

	form := Form extent: formW @ formH depth: 8.

	blt 
		sourceForm: form;
		width: form width;
		height: form height.

	"prepare transformation matrix for freetype"
	
	matrix := IntegerArray new: 4.

	"values in matrix are 16.16 fixed point floating values" 
	fix := 65536. "(2 raisedTo: 16)."
	
	matrix 
		at: 1 put: (m sx * fix) rounded;
		at: 2 put: (m shx * fix) rounded;
		at: 3 put: (m shy * fix) rounded;
		at: 4 put: (m sy * fix) rounded.

