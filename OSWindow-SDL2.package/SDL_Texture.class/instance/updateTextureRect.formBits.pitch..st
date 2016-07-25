initialize
updateTextureRect: rect formBits: bitsAndOffset pitch: pitch

	"This method is special, that takes a NBBufferWithOffset, 
	that should use form <=> texture, for updating given portion of texture
	that represents a Form. "
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	^ self nbCall: #( int SDL_UpdateTexture ( self , SDL_Rect* rect , NBBufferWithOffset bitsAndOffset , int pitch ) )