evaluating
valueWithEnoughArguments: anArray
	"call the selector with enough arguments from arguments and anArray"
	| args |
	args := Array new: selector numArgs.
	args replaceFrom: 1
		to: (arguments size min: args size)
		with: arguments
		startingAt: 1.
	args size > arguments size ifTrue: [
		args replaceFrom: arguments size + 1
			to: (arguments size + anArray size min: args size)
			with: anArray
			startingAt: 1.
	].
	^ receiver perform: selector withArguments: args