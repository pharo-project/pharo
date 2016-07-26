testing
hasParameter: aKeyword

	^ aKeyword last = $: or: [ message selector isInfix ]