copying
copyAll: selArray from: class classified: cat 
	"Install all the methods found in the method dictionary of the second 
	argument, class, as the receiver's methods. Classify the messages under 
	the third argument, cat."

	selArray do: [:s |
		(class includesLocalSelector: s) ifTrue: [
			self copy: s
				from: class
				classified: cat ] ]