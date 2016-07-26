testing
tokenish
	"Answer whether the receiver is a valid token-character -- letter, digit, underscore, or colon."

	^ self isLetter or: [ self isDigit or: [ self = $_ or: [ self = $: ] ] ]