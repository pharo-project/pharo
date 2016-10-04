private
primGetRendererInfo: info
	^ self ffiCall: #( int SDL_GetRendererInfo (self, SDL_RendererInfo* info) )