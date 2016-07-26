accessing
rationale

	^ 'You should not refference ',
		(self systemClassNames joinUsing: Character space),
		' as they are there for system purpouses and should not be refferenced directly.'