I represent a position inside a text. I am immutable.
I'm characterized by a span and position inside span.

Usually, one does not specify an absolute position in text (using textIndex accessors),
but instead a relative position, by starting from some known point (like an already known position, or text start/end),
and navigating to the desired position using the navigation protocol (moveLeft/moveRight, etc).
 
I provide operations to:
	- query the position in a text (isAtEnd, isAtStart, isValid)
	- compute a new position relative to the existing one (moveToLeft: 3, moveToUp)
	- insert new text (#insert: ) or embedded object (#insertObject:).
	
I can also give access to an absolute position in text using #asTextIndex accessors, but they are much slower/ineffective compared to relative positioning, and should not be used unless necessary.
