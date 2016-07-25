classification
isRenderer
	"A *renderer* morph transforms the appearance of its submorph in some manner. For example, it might supply a drop shadow or scale and rotate the morph it encases. Answer true if this morph acts as a renderer. This default implementation returns false."
	"Details: A renderer is assumed to have a single submorph. Renderers may be nested to concatenate their transformations. It is useful to be able to find the outer-most renderer. This can be done by ascending the owner chain from the rendered morph. To find the morph being rendered, one can descend through the (singleton) submorph lists of the renderer chain until a non-renderer is encountered."

	^ false
