*Zinc-Character-Encoding-Core
urlDecoded
	"URL Decode the receiver and return the resulting String.
	This is an encoding where characters that are illegal in a URL are escaped."

    ^ ZnPercentEncoder new decode: self