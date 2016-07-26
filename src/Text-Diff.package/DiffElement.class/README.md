My instances are container objects used by TextDiffBuilder for comparison. They hold a string and the precomputed hash of the string to speed up #=. They may reference another DiffElement object which is their pair in the diff.

Instance Variables
	hash:		<Integer>
	match:		<DiffElement>
	string:		<String>

hash
	- the hash of string, stored for fast access

match
	- another DiffElement object which has the same string and turned out to be my pair in the longest common subsequence found by a TextDiffBuilder, or nil if I don't a matching DiffElement

string
	- a part of a longer text, typically a line
