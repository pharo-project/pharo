field definition
fieldsDesc
	"
	self rebuildFieldAccessors
	"
	^#(
		void* window;
		uint framebuffer;
		uint colorbuffer;
		uint resolveFramebuffer;
 	)