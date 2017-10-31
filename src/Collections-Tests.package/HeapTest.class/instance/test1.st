tests
test1
	| data |

	"The first element of each array is the sort value, and the second will be updated by the heap with the index of the element within the heap."
	data :=  (1 to: 8) collect: [:i | {i*2. 0}].

	"Repeat with different data ordering."
	5 timesRepeat: [ | h |
		h := Heap new sortBlock: [:e1 :e2 | e1 first < e2 first].
		h indexUpdateBlock: [:array :index | array at: 2 put: index].

		data shuffled do: [:d | h add: d].
		data do: [:d | self should: (h asArray at: d second) == d].
	]