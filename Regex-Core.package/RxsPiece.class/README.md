-- Regular Expression Matcher v 1.1 (C) 1996, 1999 Vassili Bykov
--
A piece is an atom, possibly optional or repeated a number of times.

Instance variables:
	atom	<RxsCharacter|RxsCharSet|RxsPredicate|RxsRegex|RxsSpecial>
	min		<Integer>
	max		<Integer|nil> nil means infinity