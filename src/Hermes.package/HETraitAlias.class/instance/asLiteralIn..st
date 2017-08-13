converting
asLiteralIn: env
	^ TraitAlias new
		aliases: (value collect: [ :e | e asLiteralIn: env ]);
		subject: (subject asLiteralIn: env); 
		yourself