private
condenseClassOrTrait: classOrTrait
	self writeClassComment: classOrTrait.
	
	classOrTrait theNonMetaClass methodsDo: [ :method |
		(self shouldCondenseMethod: method)
			ifTrue: [ self writeMethodSource: method ]].
	
	classOrTrait theMetaClass methodsDo: [ :method |
		(self shouldCondenseMethod: method)
			ifTrue: [ self writeMethodSource: method ]]