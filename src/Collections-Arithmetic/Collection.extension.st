Extension { #name : 'Collection' }

{ #category : '*Collections-arithmetic' }
Collection >> * arg [

	"Pairwise product of two sequenceable collections of the same length."

	"#(0 1 2) * #(10 20 30) >>> #(0 20 60)"
	"{0@1. 2@3} * {10@20. 30@0} >>> {(0@20). (60@0)}"

	^ arg adaptToCollection: self andSend: #*
]

{ #category : '*Collections-arithmetic' }
Collection >> + arg [

	"Pairwise sum of two sequenceable collections of the same length."

	"#(0 1 2) + #(10 20 30) >>> #(10 21 32)"
	"{0@1. 2@3} + {10@20. 30@0} >>> {(10@21). (32@3)}"

	^ arg adaptToCollection: self andSend: #+
]

{ #category : '*Collections-arithmetic' }
Collection >> - arg [

	"Pairwise difference of two sequenceable collections of the same length."

	"#(10 20 30) - #(0 1 2) >>> #(10 19 28)"
	"{10@20. 30@0} - {0@1. 2@3} >>> {(10@19). (28@ -3)}"

	^ arg adaptToCollection: self andSend: #-
]

{ #category : '*Collections-arithmetic' }
Collection >> / arg [

	"Pairwise quotient of two sequenceable collections of the same length."

	"#(10 20 30) / #(1 3 4.0) >>> {10. (20/3). 7.5}"
	"{10@20. 30@0} / {1@3. 4.0@1} >>> {(10@(20/3)). (7.5@0)}"

	^ arg adaptToCollection: self andSend: #/
]

{ #category : '*Collections-arithmetic' }
Collection >> // arg [

	"Pairwise integer quotient of two sequenceable collections of the same length."

	"#(10 20 30) // #(1 3 4.0) >>> #(10 6 7)"
	"{10@20. 30@0} // {1@3. 4.0@1} >>> {(10@6). (7@0)}"

	^ arg adaptToCollection: self andSend: #//
]

{ #category : '*Collections-arithmetic' }
Collection >> \\ arg [

	"Pairwise integer modulo of two sequenceable collections of the same length."

	"#(10 20 30) \\ #(1 3 4.0) >>> #(0 2 2.0)"
	"{10@20. 30@0} \\ {1@3. 4.0@1} >>> {(0@2). (2.0@0)}"

	^ arg adaptToCollection: self andSend: #\\
]

{ #category : '*Collections-arithmetic-collectors' }
Collection >> abs [
	"Absolute value of all elements in the collection"

	"#(1 -2 -4 0) abs >>> #(1 2 4 0)"
	"{1@ -2. -4@0} abs >>> {(1@2). (4@0)}"

	^ self collect: [:a | a abs]
]

{ #category : '*Collections-arithmetic-collectors' }
Collection >> arcCos [
	^self collect: [:each | each arcCos]
]

{ #category : '*Collections-arithmetic-collectors' }
Collection >> arcSin [
	^self collect: [:each | each arcSin]
]

{ #category : '*Collections-arithmetic-collectors' }
Collection >> arcTan [
	^self collect: [:each | each arcTan]
]

{ #category : '*Collections-Arithmetic' }
Collection >> average [
	"Calculate the average of a collection, return a CollectionIsEmpty exception if the collection is empty. Look averageIfEmpty: aBlock"

	"#(2 6 7) average  >>> 5 "
	"#(3 5 7 7) average >>> (11/2) "
	"{3@5. 7@ -4} average >>> (5@(1/2))"

	^ self sum / self size
]

{ #category : '*Collections-Arithmetic' }
Collection >> averageIfEmpty: aBlock [

	"This method return the average of the collection if it is not empty. In the other case,
	it return the value of the block. It means the user the user of this method decide of the return value."

	"(#(10) averageIfEmpty: [ 0 ]) >>> 10"
	"(#() averageIfEmpty: [ 0 ]) >>> 0"
	"({3@5. 7@ -4} averageIfEmpty: [0@0]) >>> (5@(1/2))"
	"({} averageIfEmpty: [0@0]) >>> (0@0)"

	self ifEmpty: [ ^ aBlock value ].
	^ self average
]

{ #category : '*Collections-arithmetic-collectors' }
Collection >> ceiling [
	^ self collect: [:a | a ceiling]
]

{ #category : '*Collections-arithmetic-collectors' }
Collection >> cos [
	^self collect: [:each | each cos]
]

{ #category : '*Collections-arithmetic-collectors' }
Collection >> degreeCos [
	^self collect: [:each | each degreeCos]
]

{ #category : '*Collections-arithmetic-collectors' }
Collection >> degreeSin [
	^self collect: [:each | each degreeSin]
]

{ #category : '*Collections-arithmetic-collectors' }
Collection >> exp [
	^self collect: [:each | each exp]
]

{ #category : '*Collections-arithmetic-collectors' }
Collection >> floor [
	^ self collect: [:a | a floor]
]

{ #category : '*Collections-arithmetic-collectors' }
Collection >> ln [
	^self collect: [:each | each ln]
]

{ #category : '*Collections-arithmetic-collectors' }
Collection >> log [
	^ self collect: [:each | each log]
]

{ #category : '*Collections-arithmetic' }
Collection >> max [

	"Return the maximum value of the collection"

	"#(1 5 10 -4) max >>> 10"
	"{1@5. 10@ -4} max >>> (10@5)"

	^ self inject: self anyOne into: [ :max :each | max max: each ]
]

{ #category : '*Collections-arithmetic' }
Collection >> min [
	"Return the minimum value of the collection"

	"#(1 5 10 -4) min >>> -4"
	"{1@5. 10@ -4} min >>> (1@ -4)"

	^ self inject: self anyOne into: [:min :each | min min: each]
]

{ #category : '*Collections-Arithmetic' }
Collection >> negated [

	"Negated value of all elements in the collection"

	"#(1 2 3) negated >>> #(-1 -2 -3)"
	"{1@2. 3@ -1} negated >>> {-1@ -2. -3@1}"

	^ self collect: [ :a | a negated ]
]

{ #category : '*Collections-arithmetic' }
Collection >> raisedTo: arg [

	^ arg adaptToCollection: self andSend: #raisedTo:
]

{ #category : '*Collections-Arithmetic' }
Collection >> range [

	"returns the difference between the max and min element, their positions notwithstanding"

	"#( 1 51 10 ) range >>> 50"
	"{1@50. 10@ -10. 5@5} range >>> (9@60)"

	^ self max - self min
]

{ #category : '*Collections-Arithmetic' }
Collection >> reciprocal [

	"Return the reciever full of reciprocated elements"

	"#(1 11 0.5) reciprocal >>> {1 . 1/11 . 2.0}"
	"{10@20. (1/2)@0.5} reciprocal >>> {((1/10)@(1/20)). (2@2.0)}"

	^ self collect: [ :a | a reciprocal ]
]

{ #category : '*Collections-arithmetic-collectors' }
Collection >> roundTo: quantum [
	^self collect: [ :ea | ea roundTo: quantum ]
]

{ #category : '*Collections-arithmetic-collectors' }
Collection >> rounded [
	^ self collect: [:a | a rounded]
]

{ #category : '*Collections-Arithmetic' }
Collection >> sign [

	"Answer 1 if the receiver is greater than 0, -1 if less than 0, else 0."

	"#(5 -3.7 0) sign >>> #(1 -1 0)"

	"{5@0. -3@8. 0@ -1.} sign >>> {(1@0). (-1@1). (0@ -1)}"

	^ self collect: [ :each | each sign ]
]

{ #category : '*Collections-arithmetic-collectors' }
Collection >> sin [
	^self collect: [:each | each sin]
]

{ #category : '*Collections-arithmetic-collectors' }
Collection >> sqrt [
	^ self collect: [:each | each sqrt]
]

{ #category : '*Collections-arithmetic-collectors' }
Collection >> squared [
	^ self collect: [:each | each * each]
]

{ #category : '*Collections-Arithmetic' }
Collection >> stdev [
	| avg sample sum |
	"In statistics, the standard deviation is a measure that is used to quantify the amount of variation or dispersion of a set of data values.
	For details about implementation see comment in self sum."
	
	self size = 1 ifTrue: [ ^ 0 ].
	avg := self average.
	sample := self anyOne.
	sum := self inject: sample into: [ :accum :each | accum + (each - avg) squared ].
	sum := sum - sample.
	^ (sum / (self size - 1)) sqrt
]

{ #category : '*Collections-arithmetic' }
Collection >> sum [

	"This is implemented using a variant of the normal inject:into: pattern.
	The reason for this is that it is not known whether we're in the normal
	number line, i.e. whether 0 is a good initial value for the sum.
	Consider a collection of measurement objects, 0 would be the unitless
	value and would not be appropriate to add with the unit-ed objects."

	"#(1 2 4) sum >>> 7"

	"([#() sum] on: CollectionIsEmpty do: ['oops']) >>> 'oops'"

	"sum works with objects that understands + and - messages, like Points"

	"{1@5. 2@3. 4@7} sum >>> (7@15)"

	"or even Collections"

	"#((1 5 1) (2 3 3) (4 7 2)) sum >>> #(7 15 6)"

	| sum sample |
	self emptyCheck.
	sample := self anyOne.
	sum := self inject: sample into: [ :accum :each | accum + each ].
	^ sum - sample
]

{ #category : '*Collections-arithmetic-collectors' }
Collection >> tan [
	^self collect: [:each | each tan]
]

{ #category : '*Collections-arithmetic-collectors' }
Collection >> truncated [
	^ self collect: [:a | a truncated]
]
