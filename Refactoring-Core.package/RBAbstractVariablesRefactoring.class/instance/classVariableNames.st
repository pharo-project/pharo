transforming
classVariableNames
	| nonMetaClass |
	nonMetaClass := fromClass theNonMetaClass.
	^ (nonMetaClass allClassVariableNames collect: [ :each | each asString ]) asSet