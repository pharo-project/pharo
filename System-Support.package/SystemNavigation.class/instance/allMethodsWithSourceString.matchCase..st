query
allMethodsWithSourceString: aString matchCase: caseSensitive
	"Answer a SortedCollection of all the methods that contain, in source code, aString as a substring.  Search the class comments also"

	| list addMethod addComment |
	list := Set new.
	
	addMethod := [ :mrClass :mrSel | list add: (self createMethodNamed: mrSel realParent:  mrClass)].									
	addComment := [ :mrClass | list add: (RGCommentDefinition realClass: mrClass)].
										
	'Searching all source code...'
		displayProgressFrom: 0 to: (2*self environment classAndTraitNames size)
		during: [:bar | | count |
			count := 0.
			self allBehaviorsDo: [:each |
				bar current: (count := count + 1).
					each selectorsDo: [:sel | 
						((each sourceCodeAt: sel) includesSubstring: aString caseSensitive: caseSensitive) ifTrue: [
								 addMethod value: each value: sel]].
					(each organization classComment asString includesSubstring: aString caseSensitive: caseSensitive) ifTrue: [
								addComment value: each]	]].
			^ list asSortedCollection