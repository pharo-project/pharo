private
defaultSort: i to: j 
	"Sort elements i through j of self to be nondescending according to
	sortBlock."	"Assume the default sort block ([:x :y | x <= y])."

	| di dij dj tt ij k l n |
	"The prefix d means the data at that index."
	(n := j + 1  - i) <= 1 ifTrue: [^self].	"Nothing to sort." 
	 "Sort di,dj."
	di := array at: i.
	dj := array at: j.
	(di <= dj) "i.e., should di precede dj?"
		ifFalse: 
			[array swap: i with: j.
			 tt := di.
			 di := dj.
			 dj := tt].
	n > 2
		ifTrue:  "More than two elements."
			[ij := (i + j) // 2.  "ij is the midpoint of i and j."
			 dij := array at: ij.  "Sort di,dij,dj.  Make dij be their median."
			 (di <= dij) "i.e. should di precede dij?"
			   ifTrue: 
				[(dij <= dj) "i.e., should dij precede dj?"
				  ifFalse: 
					[array swap: j with: ij.
					 dij := dj]]
			   ifFalse:  "i.e. di should come after dij"
				[array swap: i with: ij.
				 dij := di].
			n > 3
			  ifTrue:  "More than three elements."
				["Find k>i and l<j such that dk,dij,dl are in reverse order.
				Swap k and l.  Repeat this procedure until k and l pass each other."
				 k := i.
				 l := j.
				 [[l := l - 1.  k <= l and: [dij <= (array at: l)]]
				   whileTrue.  "i.e. while dl succeeds dij"
				  [k := k + 1.  k <= l and: [(array at: k) <= dij]]
				   whileTrue.  "i.e. while dij succeeds dk"
				  k <= l]
				   whileTrue:
					[array swap: k with: l]. 
	"Now l<k (either 1 or 2 less), and di through dl are all less than or equal to dk
	through dj.  Sort those two segments."
				self defaultSort: i to: l.
				self defaultSort: k to: j]]