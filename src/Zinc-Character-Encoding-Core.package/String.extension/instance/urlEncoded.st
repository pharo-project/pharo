*Zinc-Character-Encoding-Core
urlEncoded
	"URL Encode the receiver and return the resulting String.
	This is an encoding where characters that are illegal in a URL are escaped."

    ^ ZnPercentEncoder new encode: self
