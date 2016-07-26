tests
testString
	| string |
	self assert: (self materialize: '''foo''') = 'foo'.
	self assert: (self materialize: '''FOO''') = 'FOO'.
	self assert: (self materialize: '''\u00E9l\u00E8ve en Fran\u00E7ais''') = 'élève en Français'.
	string := String withAll: { 
		$". $'. $\. Character tab. Character cr. Character lf.  Character newPage. Character backspace }.
	self assert: (self materialize: '''\"\''\\\t\r\n\f\b''') = string.
