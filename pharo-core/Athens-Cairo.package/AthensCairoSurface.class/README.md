i am a concrete implementation of Athens surface which using cairo graphics library for rendering.

Cairo library, by itself can have multiple surface types.
This class uses image surface (a bitmap located in system memory) and maps to cairo_image_surface_t* C type.

**NOTE**
As a workaround of bitblt bug, the actual Cairo surfaces, created internally is with 1 extra pixel higher than requested. This is, however completely hidden from users.