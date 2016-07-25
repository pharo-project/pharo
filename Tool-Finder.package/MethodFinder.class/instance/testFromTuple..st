tests
testFromTuple: nth
	"verify that the methods allowed don't crash the system.  Try N of each of the fundamental types.  up to 4 of each kind." 

	| objects nonRepeating even other aa cnt |
	objects := #((1 4 17 42) ($a $b $c $d) ('one' 'two' 'three' 'four')
	(x + rectangle: new) ((a b 1 4) (c 1 5) ($a 3 d) ()) (4.5 0.0 3.2 100.3)
	).

	objects := objects, {{true. false. true. false}. {Point. SmallInteger. Association. Array}.
	{Point class. SmallInteger class. Association class. Array class}.
	"{ 4 blocks }."
	{Date today. '1 Jan 1950' asDate. '25 Aug 1987' asDate. '1 Jan 2000' asDate}.
	{'15:16' asTime. '1:56' asTime. '4:01' asTime. '6:23' asTime}.
	{Dictionary new. Dictionary new. Dictionary new. Dictionary new}.
	{#(a b 1 4) asOrderedCollection. #(c 1 5) asOrderedCollection. 
		#($a 3 d) asOrderedCollection. #() asOrderedCollection}.
	{3->true. 5.6->$a. #x->2. 'abcd'->false}.
	{9 @ 3 extent: 5 @ 4. 0 @ 0 extent: 45 @ 9. -3 @ -7 extent: 2 @ 2. 4 @ 4 extent: 16 @ 16}.
	{Color red.  Color blue. Color black. Color gray}}.

	self test2: objects.
	"rec+0, rec+1, rec+2, rec+3 need to be tested.  " 
	cnt := 0.
	nth to: 4 do: [:take |
		nonRepeating := OrderedCollection new.
		objects do: [:each |
			nonRepeating addAll: (each copyFrom: 1 to: take)].
		"all combinations of take, from nonRepeating"
		even := true.
		nonRepeating combinations: take atATimeDo: [:tuple |
			even ifTrue: [other := tuple shallowCopy]
				ifFalse: [self load: (aa := Array with: tuple with: 1 with: other with: 7).
					(cnt := cnt+1) \\ 50 = 0 ifTrue: [
						Transcript cr; show: aa first printString].
					self search: true.
					self test2: aa.
					self test2: nonRepeating.
					"self test2: objects"].
			even := even not].
		].