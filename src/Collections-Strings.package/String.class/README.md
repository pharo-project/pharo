A String is an indexed collection of Characters. Class String provides the abstract super class for ByteString (that represents an array of 8-bit Characters) and WideString (that represents an array of  32-bit characters).  In the similar manner of LargeInteger and SmallInteger, those subclasses are chosen accordingly for a string; namely as long as the system can figure out so, the String is used to represent the given string.

Strings support a vast array of useful methods, which can best be learned by browsing and trying out examples as you find them in the code.

Here are a few useful methods to look at...
	String match:
	String contractTo:

String also inherits many useful methods from its hierarchy, such as
	SequenceableCollection ,
	SequenceableCollection copyReplaceAll:with:
