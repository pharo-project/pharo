documentation
documentation
	<ignoreForCoverage>
	"self showDocumentation"
	
	^ '"This package provides functionality for splitting and joining strings similarly to that offered by Perl, Python and Ruby. In addition, split and join work for any kind of sequence, and offer the possibility to split strings using regular expressions.

Suppose object A is a SequenceableCollection of elements of type T (let''s call its type S[T]), and B is a splitter of type either T or S[T]. Then A splitOn: B yields an object C of type OC[S[T]] (OC=OrderedCollection). B can be either an element or a subsequence, so:"

	''abracadabra'' splitOn: $b.
	"-> an OrderedCollection(''a'' ''racada'' ''ra'')"

	''abracadabra'' splitOn: ''ca''.
	"-> an OrderedCollection(''abra'' ''dabra'')"

"A splitOn: B is the same as B split: A, so:"

	''ab'' split: ''abracadabra''.
	"-> an OrderedCollection('''' ''racad'' ''ra'')"

"joinUsing: and join: are the inverse of splitOn: and split:. You may use either of the forms:
	C joinUsing: B
or:
	B join: C
The result is normally the same type as the joiner. If the joiner (B) is of type S[T] and C is of type OC[S[T]] or Array[S[T]], the result is also of type S[T]. For example:"

	'' loves '' join: #(''john'' ''jane'' ''jack'').
	"-> ''john loves jane loves jack''"

"An object that is not a sequence can also be used as a joiner, in which case the final result is an array. Also the collection being joined may contain elements that are not sequences, in which case they are treated as singleton arrays."

	1 join: ''hello''.
	"-> #($h 1 $e 1 $l 1 $l 1 $o)"

"Note that in the case of characters and strings being used as joiners, the result is always a string."

"In general:
	((A splitOn: B) joinUsing: B) = A
or:
	(B join: (B split: A)) = A
so:"

	(''r'' join: (''r'' split: ''abracadabra'')) = ''abracadabra''.
	"->  true"

"Although the main application is for Strings, split and join will work for any kinds of SequenceableCollections, including Arrays and Intervals."

	[:n | n isPrime] split: (2 to: 20).
	"-> an OrderedCollection(#() #() #(4) #(6) #(8 9 10) #(12) #(14 15 16) #(18) #(20))"

"In addition, the following special cases are supported:

- split an S[T] using a block of type T->Boolean:"

	''abracadabra'' splitOn: [:x | ''bc'' includes: x].
	"-> an OrderedCollection(''a'' ''ra'' ''ada'' ''ra'')"

"- split a String using a regex:"

	''abracadabra'' splitOn: ''[bcdr]+'' asRegex.
	"-> an OrderedCollection(''a'' ''a'' ''a'' ''a'' ''a'')"

"- join all elements of an S[T] yielding another S[T]: '' ''- "

	''abracadabra'' joinUsing: '':''.
	" -> ''a:b:r:a:c:a:d:a:b:r:a''"

"Note that in these cases the rule that (B join: (B split: A)) = A does not hold, either because B maps to different elements, or because there is no A to start with."

	$: join: (1 to: 4).
	"-> ''1:2:3:4''"

"Finally, there is the convenient utility method joinUsing:last:, used as follows:"

	(1 to: 5) joinUsing: '', '' last: '' and ''.
	"-> ''1, 2, 3, 4 and 5''"

"This package merges and generalizes functionality from the Join package by Keith Hodges and the RubyShards package by Damien Pollet and Oscar Nierstrasz. "
'