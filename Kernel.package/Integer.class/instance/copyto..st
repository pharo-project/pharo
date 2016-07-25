private
copyto: x
	| stop |
	stop := self digitLength min: x digitLength.
	^ x replaceFrom: 1 to: stop with: self startingAt: 1