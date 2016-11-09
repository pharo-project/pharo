private
setFrom: startInteger to: stopInteger by: stepInteger

	start := startInteger.
	stop := stopInteger.
	step := stepInteger.
	step isZero ifTrue: [ ^ DomainError signal: 'Zero size steps not allowed' ]