Find a method in the system from a set of examples.  Done by brute force, trying every possible selector.  Errors are skipped over using ( [3 + 'xyz'] ifError: [^ false] ).
Submit an array of the form ((data1 data2) answer  (data1 data2) answer).

	MethodFinder methodFor: #( (4 3) 7  (0 5) 5  (5 5) 10).

answer:  'data1 + data2'

More generally, use the brace notation to construct live examples.

The program tries data1 as the receiver, and
	tries all other permutations of the data for the receiver and args, and
	tries leaving out one argument, and
	uses all selectors data understands, and
	uses all selectors in all od data's superclasses.

Floating point values must be precise to 0.01 percent, or (X * 0.0001).

If you get an error, you have probably discovered a selector that needs to be removed from the Approved list.  See MethodFinder.initialize.  Please email the Pharo Team.

Only considers 0, 1, 2, and 3 argument messages.  The argument data may have 1 to 5 entries, but only a max of 4 used at a time.  For now, we only test messages that use given number of args or one fewer.  For example, this data (100 true 0.6) would test the receiver plus two args, and the receiver plus one arg, but not any other patterns.

Three sets of selectors:  Approved, AddAndRemove, and Blocks selectors.  When testing a selector in AddAndRemove, deepCopy the receiver.  We do not handle selectors that modify an argument (printOn: etc.).  Blocks is a set of (selector argNumber) where that argument must be a block.

For perform, the selector is tested.  It must be in the Approved list.

do: is not on the Approved list.  It does not produce a result that can be tested.  Type 'do' into the upper pane of the Selector Finder to find messages list that.


Implementation Notes
	
	arguments of the load: method are structured as follows:
		- Odd list entries are data for it, even ones are the answers.  nil input means data and answers were supplied already."
		"(MethodFinder new) load: #( (4 3) 7  (-10 5) -5  (-3 11) 8)





