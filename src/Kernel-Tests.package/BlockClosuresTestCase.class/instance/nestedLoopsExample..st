examples
nestedLoopsExample: arrays

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
