public
commentForIssues: issues
	"self new commentForIssues: #(12623 12621 12615)"
	
	^ String streamContents: [ :stream | 
		issues 
			do: [ :each | | description link |
				stream print: each; space.
				description := ZnClient new
					url: 'http://tracker.pharo.org/issues/name/';
					addPath: each asString;
					get.
				stream << (description reject: [ :c | c = $' ]); cr.
				link := 'https://pharo.fogbugz.com/f/cases' asUrl / each asString.
				stream tab; print: link; cr; cr ]
			displayingProgress: 'Fetching Issue Descriptions' ]