private
methodsIn: aPackage
	aPackage ifNil: [ ^ #() ].
	^ aPackage methods reject: [ :method | 
		(self ignoredSelectors includes: method selector)
			or: [ method compiledMethod isAbstract
			or: [ method compiledMethod refersToLiteral: #ignoreForCoverage ] ] ]