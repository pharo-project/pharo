converting
asString
	"Convert to a String with Characters for each byte.
	Fast code uses primitive that avoids character conversion"

	^ (String new: self size) replaceFrom: 1 to: self size with: self