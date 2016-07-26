process specific
updateInheritableKeys
"
	self updateInheritableKeys
"
	| keys |
	keys := Set new.
	ProcessSpecificVariable allSubclasses select: [ :each | each isInheritable ] thenDo: [ :each | keys add: each soleInstance index].
	  
	InheritablePSKeys := keys asArray ifEmpty: [ nil ]