initialization
initialize
	super initialize.
	self rewriteRule
		replace: '`@receiver ifNotEmptyDo: `@statements'
			with: '`@receiver ifNotEmpty: `@statements';	
		replace: '`@receiver ifNotEmptyDo: `@statements1 ifEmpty: `@statements2'
			with: '`@receiver ifNotEmpty: `@statements1 ifEmpty: `@statements2';
		replace: '`@receiver ifEmpty: `@statements1 ifNotEmptyDo: `@statements2'
			with: '`@receiver ifEmpty: `@statements1 ifNotEmpty: `@statements2'
		