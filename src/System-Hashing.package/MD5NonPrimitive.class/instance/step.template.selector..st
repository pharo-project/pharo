private-functions
step: data template: item selector: selector 
	"Perform one step in the round"
	| args |
	args := { 
		(state at: (item at: 1)).
		(state at: (item at: 2)).
		(state at: (item at: 3)).
		(state at: (item at: 4)).
		(data at: (item at: 5)).
		(item at: 6).
		(item at: 7)
	 }.
	self 
		perform: selector
		withArguments: args