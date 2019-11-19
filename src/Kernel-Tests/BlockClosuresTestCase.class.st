"
This test case collects examples for block uses that require full block closures.
"
Class {
	#name : #BlockClosuresTestCase,
	#superclass : #TestCase,
	#category : #'Kernel-Tests-Methods'
}

{ #category : #'jensen device examples' }
BlockClosuresTestCase >> comment [

  "  The Jensen device was something very sophisticated in the days of Algol 60.
Essentially it was tricky use of a parameter passing policy that was called 'call by name'. In modern terminology, a call by name parameter was a pair of blocks (in a system with full block closures, of course.)

For the lovers of Algol 60, here is a short example:

 BEGIN
  REAL PROCEDURE JensenSum (A, I, N);
     REAL  A;   INTEGER   I, N;
  BEGIN
    REAL  S;
    S := 0.0;
    FOR I := 1 STEP 1 UNTIL N DO  S := S + A;
  JensenSum := S;
  END;

  ARRAY X [1:10], Y[1:10, 1:10];
  COMMENT Do array initialization here ;

  JensenSum (X[I], I, 10);
  JensenSum (Y[I, I], I, 10);
  JensenSum(JensenSum(Y[I, J], J, 10), I, 10);
END;

The first call sums the elements of X, the second sums the diagonal elements of Y and the third call sums up all elements of Y.

It is possible to reimplement all this with blocks only and that is what is done in the jensen device examples.

Additional remark:
The Jensen device was something for clever minds. I remember an artice written by Donald Knuth and published in the Communications of the ACM (I think in 1962, but I may err) about that programming trick. That article showed how a simple procedure (called the general problem solver) could be used to do almost anything. The problem was of course to find out the right parameters. I seached my collection of photocopies for that article, but regrettably I could not find it. Perhaps I can find it later.
 "
]

{ #category : #examples }
BlockClosuresTestCase >> constructCannotReturnBlockInDeadFrame [

	^ [:arg | ^arg].

]

{ #category : #examples }
BlockClosuresTestCase >> constructFibonacciBlockInDeadFrame [

	| fib |
	fib := [:val | 
		(val <= 0) ifTrue: [self error: 'not a natural number'].
		(val <= 2) ifTrue: [1]
		     ifFalse: [(fib value: (val - 1)) + (fib value: (val - 2))]].
	^fib

]

{ #category : #examples }
BlockClosuresTestCase >> constructFibonacciBlockWithBlockArgumentInDeadFrame [

	^ [:val :blk | 
		(val <= 0) ifTrue: [self error: 'not a natural number'].
		(val <= 2) ifTrue: [1]
		    ifFalse: [(blk value: (val - 1) value: blk) + (blk value: (val - 2) value: blk)]].

]

{ #category : #examples }
BlockClosuresTestCase >> constructSharedClosureEnvironmentInDeadFrame [

	|array result| 
	result := 10.
	array := Array new: 2.
	array at: 1 put: [:arg | result := arg].
	array at: 2 put: [result].
	^array

]

{ #category : #examples }
BlockClosuresTestCase >> continuationExample1: aCollection [

  " see comment below.
    Here we simply collect the values of a value with continuation block "


     | streamCreator collector |

   streamCreator := [:collection | | i localBlock |
                i := 1.
                localBlock :=
                    [  | current |
                     current := collection at: i.
                     i := i + 1.
                     Array with: current
                           with: (i<= collection size ifTrue: [localBlock]
                                                      ifFalse: [nil])
                    ].
             ].
 

 collector := [:valueWithContinuation |  | oc |
                 oc := OrderedCollection new.
                 [ | local |
                  local := valueWithContinuation value.
                  oc add: local first.
                  local last notNil]
                 whileTrue: [].
                 oc.
               ].

  ^collector value: (streamCreator value: aCollection).

"The continuation examples are examples of a 'back to LISP' style.
These examples use blocks to process the elements of a collection in a
fashion that is similar to streaming.
The creator block creates a blocks that act like a stream. In the
following, this block is called a 'value with continuation block'.
When such a value with continuation block receives the message
value, it returns a Array of two elements, the value and the
continuation
 1. the next collection element
 2. a so-called continuation, which is either nil or a block
    that can return the next value with continuation.
    
To collect all elements of a value with continuation stream,
use the collector block. "

]

{ #category : #examples }
BlockClosuresTestCase >> continuationExample2: aCollection [

  " see comment in method continuationExample1:.
    The block named 'processor' takes a value with contiuation
    and a processing block. It creates a new value with continuation.
   Again we use a collector to collect all values.  "

     | stream processor collector |

   stream := [:collection | | i localBlock |
                i := 1.
                localBlock :=
                    [  | current |
                     current := collection at: i.
                     i := i + 1.
                     Array with: current
                           with: (i<= collection size ifTrue: [localBlock]
                                                      ifFalse: [nil])
                    ].
             ].
 
  processor := 
            [:valueWithContinuation :activity | | localBlock |
                localBlock :=
                 [ | current |
                   current := valueWithContinuation value. 
                   Array with: (activity value: current first)
                         with: (current last notNil ifTrue: [localBlock])].
               localBlock
           ].

 collector := [:valueWithContinuation |  | oc |
                 oc := OrderedCollection new.
                 [ | local |
                  local := valueWithContinuation value.
                  oc add: local first.
                  local last notNil]
                 whileTrue: [].
                 oc.
               ].

  ^collector value: (processor value: (stream value: aCollection)
                                           value: [:x | x * x]).
]

{ #category : #examples }
BlockClosuresTestCase >> continuationExample3: aCollection [

  " see comment in method continuationExample1:.
    The block named 'processor' takes a value with contiuation
    and a processing block. It creates a new value with continuation.
    Here we set up a chain of three values with continuation:
    one data source and two value processors.
    Again we use a collector to collect all values.  "

    | stream processor collector |

   stream := [:collection | | i localBlock |
                i := 1.
                localBlock :=
                    [  | current |
                     current := collection at: i.
                     i := i + 1.
                     Array with: current
                           with: (i<= collection size ifTrue: [localBlock]
                                                      ifFalse: [nil])
                    ].
             ].
 
  processor := 
            [:valueWithContinuation :activity | | localBlock |
                localBlock :=
                 [ | current |
                   current := valueWithContinuation value. 
                   Array with: (activity value: current first)
                         with: (current last notNil ifTrue: [localBlock])].
               localBlock
           ].

 collector := [:valueWithContinuation |  | oc |
                 oc := OrderedCollection new.
                 [ | local |
                  local := valueWithContinuation value.
                  oc add: local first.
                  local last notNil]
                 whileTrue: [].
                 oc.
               ].

  ^collector value: (processor value: (processor value: (stream value: aCollection)
                                                                     value: [:x | x * x])
                                            value: [:x | x - 10]).
]

{ #category : #examples }
BlockClosuresTestCase >> example1: anInteger [

  " this example is very simple. A named block recursively computes the factorial.
    The example tests whether the value of x is still available after the recursive call.
    Note that the recursive call precedes the multiplication. For the purpose of the test
    this is essential. (When you commute the factors, the example will work also in
    some system without block closures, but not in Pharo.) "
	<sampleInstance>

    | factorial |

   factorial := [:x | x = 1 ifTrue: [1]
                            ifFalse: [(factorial value: x - 1)* x]].
  ^ factorial value: anInteger


  
]

{ #category : #examples }
BlockClosuresTestCase >> example2: anInteger [

  " BlockClosuresTestCase new example2: 6"
  " to complicate the example1, we set up a dynamic reference chain that is
    used to dump all calls of facorial when recursion depth is maximal.
    The return value is an instance of orderedCollection, the trace. "
	<sampleInstance>

    | factorial trace |

   trace := OrderedCollection new.
   factorial :=
       [:x :dumper :trace2 |  | localDumper |
          localDumper := [ :collection |
                             collection add: x. 
                             dumper value: collection.].
            x = 1 ifTrue:
                    [localDumper value: trace2.
                     1]
                  ifFalse:
                    [(factorial value: x - 1
                                 value: localDumper
                                 value: trace2)* x.
                   ]
         ].
  factorial value: anInteger
             value: [ :collection | ]
             value: trace.
  ^trace
]

{ #category : #'jensen device examples' }
BlockClosuresTestCase >> gpsExample1: aCollection [

   " BlockClosuresTestCase new gpsExample1: (1 to: 100) asArray"

 | gps i  s |

  gps := [:idx :exp :sum | | cnt |
               cnt := 1.
               sum first value: 0.
               [idx first value: cnt.
                sum first value: (sum last value + exp last value). 
                cnt := cnt + 1.
                cnt <= aCollection size] whileTrue: [   ].
              sum last value
             ].


  ^gps value: (Array with: [:val | i := val]
                    with: [ i])
      value: (Array with: [:val | aCollection at: i put:  val]
                    with: [ aCollection at: i])
      value: (Array with: [:val | s := val]
                    with: [ s])

]

{ #category : #'jensen device examples' }
BlockClosuresTestCase >> gpsExample2: aCollection [

   " BlockClosuresTestCase new 
       gpsExample2: #(#(1 2 3 4 5) #(6 7 8 9 10) #(11 12 13 14 15) #(16 17 18 19 20) #(21 22 23 24 25))"

    | js i j |

  "  js is the translation of the Algol procedure from method comment. "
  js := [:a :idx :n | | sum |
               sum := 0.
               idx first value: 1.
               [idx last value <= n last value]
                 whileTrue:
                   [sum :=  sum  + a last value. 
                    idx first value:  idx last value + 1.].
              sum
             ].

  "  This is the most complicated call that is mentioned in method comment. Note that  js  is called recursively. "

  ^ js value: (Array with: [:val | self error: 'can not assign to procedure']
                        with: [ js value: (Array with: [:val | (aCollection at: i) at: j put: val]
                                                     with: [ (aCollection at: i) at: j])
                                   value: (Array with:[:val | j := val]
                                                     with: [ j])
                                   value: (Array with: [:val | self error: 'can not assign to constant']
                                                     with: [ aCollection size])
                               ]
               )
    value: (Array with:[:val | i := val]
                  with: [ i])
    value: (Array with: [:val | self error: 'can not assign to constant']
                  with: [ aCollection size])


]

{ #category : #examples }
BlockClosuresTestCase >> nestedLoopsExample: arrays [

 " A while ago, Hans Baveco asked for a way to
   dynamically nest loops. Better solutions than this one
   were proposed, but this one is a beautiful test for
   recursive block usage. "
 
  | result sizeOfResult streams block |


"arrays := OrderedCollection new.
arrays add: #(#a #b);
       add: #(1 2 3 4);
       add: #('w' 'x' 'y' 'z')."
sizeOfResult :=
   arrays inject: 1 into:
          [:prod :array | prod * array size].
streams := arrays collect:
			[:a | a readStream]. " This is an OrderedCollection of Streams "

result := OrderedCollection new: sizeOfResult.
block :=
 [:r :tupel :allStreams | | innerBlock |
   innerBlock :=
    [:myIdx |
       [myIdx = allStreams size
	  ifTrue: [1 to: allStreams size do:
		   [:i | tupel at: i put: (allStreams at: i) peek].
			 r addLast: tupel shallowCopy]
	 ifFalse:  [innerBlock value: myIdx + 1].
	(allStreams at: myIdx) next.     
	(allStreams at: myIdx) atEnd
      ] 
       whileFalse: [].
     (allStreams at: myIdx) reset.
    ].
    innerBlock value: 1.
   r   
  ].
 block value: result 
         value: (Array new: streams size) " this is a buffer "
         value: streams.

  ^result

]

{ #category : #tests }
BlockClosuresTestCase >> testCannotReturn [

	| blk |
	blk := self constructCannotReturnBlockInDeadFrame.
	self 
		should: [blk value: 1]
		raise: Exception

]

{ #category : #tests }
BlockClosuresTestCase >> testContinuationExample1 [
	| array |
	array := (1 to: 20) asOrderedCollection.
	self assert: (self continuationExample1: array) equals: array
]

{ #category : #tests }
BlockClosuresTestCase >> testContinuationExample2 [
	| array |
	array := (1 to: 20) asOrderedCollection.
	self assert: (self continuationExample2: array) equals: (array collect: [ :x | x * x ])
]

{ #category : #tests }
BlockClosuresTestCase >> testContinuationExample3 [
	| array |
	array := (1 to: 20) asOrderedCollection.
	self assert: (self continuationExample3: array) equals: (array collect: [ :x | x * x - 10 ])
]

{ #category : #tests }
BlockClosuresTestCase >> testExample2 [
	self assert: (self example2: 5) equals: (1 to: 5) asOrderedCollection
]

{ #category : #tests }
BlockClosuresTestCase >> testGpsExample1 [
	| result array |
	array := (1 to: 100) asArray.
	result := array inject: 0 into: [ :sum :val | sum + val ].
	self assert: (self gpsExample1: array) equals: result
]

{ #category : #tests }
BlockClosuresTestCase >> testGpsExample2 [
	| result array |
	"  integer matrix elements should be used for the purpose of this test. "
	array := #(#(1 2 3 4 5) #(6 7 8 9 10) #(11 12 13 14 15) #(16 17 18 19 20) #(21 22 23 24 25)).
	result := array inject: 0 into: [ :sum :subarray | sum + (subarray inject: 0 into: [ :s :elem | s + elem ]) ].
	self assert: (self gpsExample2: array) equals: result
]

{ #category : #tests }
BlockClosuresTestCase >> testNestedLoopsExample1 [
	| arrays result |
	arrays := OrderedCollection new.
	arrays
		add: #(#a #b);
		add: #(1 2 3 4);
		add: #('w' 'x' 'y' 'z').
	result := OrderedCollection new.
	CollectionCombinator new forArrays: arrays processWith: [ :item | result addLast: item ].
	self assert: (self nestedLoopsExample: arrays) equals: result
]

{ #category : #tests }
BlockClosuresTestCase >> testReentrantBlock [
	| fib |
	fib := [ :val | 
	val <= 0 ifTrue: [ self error: 'not a natural number' ].
	val <= 2 ifTrue: [ 1 ] ifFalse: [ (fib value: val - 1) + (fib value: val - 2) ] ].

	self should: [ fib value: 0 ] raise: self classForTestResult error.
	self assert: (fib value: 1) equals: 1.
	self assert: (fib value: 2) equals: 1.
	self assert: (fib value: 3) equals: 2.
	self assert: (fib value: 4) equals: 3.
	self assert: (fib value: 5) equals: 5.
	self assert: (fib value: 6) equals: 8
]

{ #category : #tests }
BlockClosuresTestCase >> testReentrantBlockOldEnvironment [
	| fib |
	fib := self constructFibonacciBlockInDeadFrame.
	self should: [ fib value: 0 ] raise: self classForTestResult error.
	self assert: (fib value: 1) equals: 1.
	self assert: (fib value: 2) equals: 1.
	self assert: (fib value: 3) equals: 2.
	self assert: (fib value: 4) equals: 3.
	self assert: (fib value: 5) equals: 5.
	self assert: (fib value: 6) equals: 8
]

{ #category : #tests }
BlockClosuresTestCase >> testReentrantBlockOldEnvironmentWithBlockArguement [
	| fib |
	fib := self constructFibonacciBlockWithBlockArgumentInDeadFrame.
	self should: [ fib value: 0 value: fib ] raise: self classForTestResult error.
	self assert: (fib value: 1 value: fib) equals: 1.
	self assert: (fib value: 2 value: fib) equals: 1.
	self assert: (fib value: 3 value: fib) equals: 2.
	self assert: (fib value: 4 value: fib) equals: 3.
	self assert: (fib value: 5 value: fib) equals: 5.
	self assert: (fib value: 6 value: fib) equals: 8
]

{ #category : #tests }
BlockClosuresTestCase >> testSharedClosureEnvironment [
	| blockArray |
	blockArray := self constructSharedClosureEnvironmentInDeadFrame.
	self assert: (blockArray at: 2) value equals: 10.
	self assert: ((blockArray at: 1) value: 5) equals: 5.
	self assert: (blockArray at: 2) value equals: 5
]
