updating screen
updateRectangle: rectangle
	| intersection sdlRectangle bitsAndOffset pitch x y w h |
	intersection := rectangle
		intersect: (0 @ 0 corner: window extent)
		ifNone: [ ^ self ].
	self validate
		ifFalse: [ ^ self ].
	x := intersection left asInteger.
	y := intersection top asInteger.
	w := intersection width asInteger.
	h := intersection height asInteger.
	sdlRectangle := SDL_Rect
		newX: x
		y: y
		w: w
		h: h.
	pitch := form width * 4.
	self error: 'This needs to be redo with new FFI-NB'.
	"bitsAndOffset := NBBufferWithOffset new
		buffer: form bits;
		offset: y * pitch + (x * 4)."
	texture
		updateTextureRect: sdlRectangle
		formBits: bitsAndOffset
		pitch: pitch.
	deferUpdates
		ifTrue: [ ^ self ].
	renderer copy: texture srcRect: sdlRectangle dstRect: sdlRectangle.
	renderer present