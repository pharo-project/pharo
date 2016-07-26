field definition
fieldsDesc

" self rebuildFieldAccessors

A structure from SurfacePlugin.h 
 

typedef struct sqSurfaceDispatch {
	/* Version information. Must be provided by the client
	   so the surface manager can check if certain operations
	   are supported. */
	int majorVersion;
	int minorVersion;

	/* Version 1.0 */
	fn_getSurfaceFormat getSurfaceFormat;
	fn_lockSurface lockSurface;
	fn_unlockSurface unlockSurface;
	fn_showSurface showSurface;
} sqSurfaceDispatch; 
"
	^ #(
		int majorVersion;
		int minorVersion;
		FFICallback getSurfaceFormatFn;
		FFICallback lockSurfaceFn;
		FFICallback unlockSurfaceFn;
		FFICallback showSurfaceFn
	)