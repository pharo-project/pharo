accessing
rationale
	^ 'Checks if messages sent to self or super exist in the hierarchy, since these can be statically typed. Reported methods will certainly cause a doesNotUnderstand: message when they are executed.'