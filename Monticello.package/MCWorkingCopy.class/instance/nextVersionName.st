private
nextVersionName
	| branch oldName base author |
	branch := ''.
	ancestry ancestors isEmpty
		ifTrue: [counter ifNil: [counter := 0]. base := package name]
		ifFalse:
			[oldName := ancestry ancestors first name.
			oldName last isDigit
				ifFalse: [base := oldName]
				ifTrue: [
					base := oldName copyUpToLast: $-.
					branch := ((oldName copyAfterLast: $-) copyUpToLast: $.) copyAfter: $. ].
			counter ifNil: [
				counter := (ancestry ancestors collect: [:each |
					each name last isDigit
						ifFalse: [0]
						ifTrue: [(each name copyAfterLast: $.) extractNumber]]) max]].

	branch isEmpty ifFalse: [branch := '.',branch].
	counter := counter + 1.
	author := Author fullName collect: [ :each | each isAlphaNumeric ifTrue: [ each ] ifFalse: [ $_ ] ].
	^ base , '-' , author , branch , '.' , counter asString