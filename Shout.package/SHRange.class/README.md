I associate a type with a range of characters in a String
I have these instance variables...
	start - the one based index of the first character of the range within the String.
	end - the one based index of the last character  of the range within the String.
	type - a Symbol describing the type of the range
	
A sequence of instances of me are created by an instance of SHParserST80 which can then used by an instance of  SHTextStyler to style Text. 