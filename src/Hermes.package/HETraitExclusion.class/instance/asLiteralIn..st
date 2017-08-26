converting
asLiteralIn: env
	^ TraitExclusion new
		exclusions: (value collect: [ :e | e asLiteralIn: env ]);
		subject: (subject asLiteralIn: env);
		yourself