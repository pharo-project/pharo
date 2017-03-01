converting
asForm
	"create a form and copy an image data there"
	self checkSession.
	self flush.
 	^ (AthensCairoSurfaceForm extent: (self width@(self height + 1)) depth: 32 bits: id)
		surface: self;
		yourself