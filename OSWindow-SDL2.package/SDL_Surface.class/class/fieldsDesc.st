fields description
fieldsDesc
	"
	self initializeAccessors
	"
	^#(
		Uint32 flags;
		SDL_PixelFormat* format;
    		int w;
		int h;
    		int pitch;
    		void* pixels;
		void* userdata;
		int locked;
		void* lock_data;
		SDL_Rect clip_rect;
    		void *map;
    		int refcount;
 	)