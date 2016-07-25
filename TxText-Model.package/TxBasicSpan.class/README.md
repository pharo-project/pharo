I am an abstract class, representing a portion of text (span).
I form a double-linked list with my neighbour spans and our full list forms the contents of text.

Spans have a size and can have attributes. Size represents the number of positions within a span, which always =  size + 1.
For instance a character span with 2 characters 'AB' , can have 3 different positions:
 |AB
 A|B
 AB|
where '|' shows the position in the span and ranges from 0 to 2.
In this way TxTextPosition can navigate a span (using moveLeft/moveRight commands),
without actually needing to deal with strings, characters or anything else.