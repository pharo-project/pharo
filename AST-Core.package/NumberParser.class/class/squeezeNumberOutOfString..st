instance creation
squeezeNumberOutOfString: stringOrStream
	"Try and find a number in this string. First, look if the string 
	starts with a number. Then, see if it ends with a number. Then,
	remove a character from the front and see if the remaining 
	string makes a number. Repeat the process until no characters
	are left or the number has been found. As soon as a number is
	found, it is returned. Otherwise, the method fails."
	^ self squeezeNumberOutOfString: stringOrStream onError: [self error: 'Reading a number failed']