private
sourceRegexSearch: aSearchString
	| regex |
	regex := aSearchString asRegex.
	^ self methodSearch: [ :method | regex search: method sourceCode ]