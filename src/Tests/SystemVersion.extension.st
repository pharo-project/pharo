Extension { #name : #SystemVersion }

{ #category : #'*Tests' }
SystemVersion class >> fromVersionString: aString [
	| matcher instance minor major patch |

	matcher := '(([A-Z][a-zA-Z]*)[-]*([0-9]+)(\.([0-9]+))?(\.([0-9]+))?)(([+](([a-zA-Z]*)([.]*build.([0-9]+)(.sha.([0-9a-z]+))*)*))*)' asRegex.
	(matcher matches: aString)
		ifFalse: [ ^ self error: 'Not a version string' ].

	major := 	(matcher subexpression: 4) asNumber.
	minor := (matcher subexpression: 6) isString ifTrue: [ (matcher subexpression: 6) asNumber ] ifFalse: [ 0 ].
	patch := (matcher subexpression: 8) isString ifTrue: [ (matcher subexpression: 8) asNumber ] ifFalse: [ 0 ].
	instance := self major: major minor: minor patch: patch.
	instance type: (matcher subexpression: 3).
	(matcher subexpression: 12) isString ifTrue: [ instance suffix: (matcher subexpression: 12) ].
	(matcher subexpression: 14) isString ifTrue: [ instance build: (matcher subexpression:  14) asNumber ].
	(matcher subexpression: 16) isString ifTrue: [ instance commitHash: (matcher subexpression:  16) ].
	^ instance
]
