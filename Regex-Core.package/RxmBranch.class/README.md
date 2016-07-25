-- Regular Expression Matcher v 1.1 (C) 1996, 1999 Vassili Bykov
--
This is a branch of a matching process. Either `next' chain should match, or `alternative', if not nil, should match. Since this is also used to build loopbacks to match repetitions, `loopback' variable indicates whether the instance is a loopback: it affects the matcher-building operations (which of the paths through the branch is to consider as the primary when we have to find the "tail" of a matcher construct).

Instance variables
	alternative		<RxmLink> to match if `next' fails to match.
	loopback		<Boolean>