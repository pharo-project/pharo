I am a private class, used to represent a 'cairo_font_face_t *' data structure 
of Cairo library.

I am not useful for direct use nor providing any functionality. My only purpose is to keep a strong reference to original FT2Face object
(so it won't be freed before a corresponding instance of mine will release it).

