accessing
setExtend: aSymbol
	"
	typedef enum {
    CAIRO_EXTEND_NONE,
    CAIRO_EXTEND_REPEAT,
    CAIRO_EXTEND_REFLECT,
    CAIRO_EXTEND_PAD
} cairo_extend_t;
	"
	| enum |
	enum := self defaultExtend.
	aSymbol = #None ifTrue: [ enum := CAIRO_EXTEND_NONE ].
	aSymbol = #Repeat ifTrue: [ enum := CAIRO_EXTEND_REPEAT ].
	aSymbol = #Reflect ifTrue: [ enum := CAIRO_EXTEND_REFLECT ].
	aSymbol = #Pad ifTrue: [ enum := CAIRO_EXTEND_PAD ].
	self primSetExtend: enum.