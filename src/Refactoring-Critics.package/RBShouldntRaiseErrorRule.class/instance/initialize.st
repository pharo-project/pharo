initialization
initialize
	super initialize.
	self rewriteRule
		replace: 'self shouldnt: [ `@statements ] raise: Error' with: '`@statements';
		replace: 'self shouldnt: [ `@statements ] raise: Exception' with: '`@statements'