accessing context
contextToSelectFrom: aSession
	^ aSession assertionCallerContext ifNil: [ aSession testContext ]