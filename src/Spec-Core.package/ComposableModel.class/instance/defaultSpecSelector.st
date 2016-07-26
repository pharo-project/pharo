private
defaultSpecSelector

	self class class withAllSuperclassesDo: [ :class |
		(((SpecPragmaCollector behavior: class)
			filter: [ :pragma | 
				pragma keyword = 'spec:' and: [ pragma arguments includes: #default ]])
			reset;
			collected)
				ifNotEmpty: [ :pragmas | ^ pragmas first method selector ]].

	self specSelectors
		ifNotEmpty: [:col | col size = 1 ifTrue: [ ^ col first ]].
		
	"should use pragmas"
	^ #defaultSpec
	
