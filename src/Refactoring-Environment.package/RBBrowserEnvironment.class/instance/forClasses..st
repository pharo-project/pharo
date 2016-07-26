environments
forClasses: classCollection 
	| classes |
	classes := OrderedCollection new: classCollection size * 2.
	classCollection do: 
			[:each | 
			classes add: each theNonMetaClass;
				add: each theMetaClass].
	^RBClassEnvironment onEnvironment: self classes: classes