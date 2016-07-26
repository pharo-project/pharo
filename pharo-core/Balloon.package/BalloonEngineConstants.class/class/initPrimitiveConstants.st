pool definition
initPrimitiveConstants
	"Initialize the primitive constants"

	"Primitive type constants"
	GEPrimitiveUnknown := 0.
	GEPrimitiveEdgeMask := 16rFF.
	GEPrimitiveFillMask := 16rFF00.
	GEPrimitiveTypeMask := 16rFFFF.

	"General state constants (Note: could be compressed later)"
	GEObjectType := 0.				"Type of object"
	GEObjectLength := 1.			"Length of object"
	GEObjectIndex := 2.			"Index into external objects"
	GEObjectUnused := 3.			"Currently unused"

