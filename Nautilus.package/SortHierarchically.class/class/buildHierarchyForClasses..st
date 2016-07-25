instance creation
buildHierarchyForClasses: aCollection
	
	| classes roots dct indenter result |

	indenter := nil.
	classes := aCollection asIdentitySet.
	roots := OrderedCollection new.
	dct := IdentityDictionary new.		
	
	classes do: [ :cls | | su found |
		su := cls .
		found := false.
		[ found or: [ su isNil ] ] whileFalse: [ 
			su := su superclass.
			(classes includes: su) ifTrue: [ 
					found := true.
					(dct at: su ifAbsentPut: [ OrderedCollection new ]) add: cls.
				].
		].
		found ifFalse: [ roots add: cls ].
	].

	result := OrderedCollection new.
	indenter := [ :col :indent | 
		(col sort: [ :a :b | a name <= b name ]) do: [ :each | 
			result add: (SortHierarchicallyIndentNode on: each indentBy: indent). 
			dct at: each ifPresent: [ :subCol | indenter value: subCol value: indent + 1 ]
		] 
	].

	indenter value: roots value: 0.

	^ self new
		setHierarchy: result;
		yourself