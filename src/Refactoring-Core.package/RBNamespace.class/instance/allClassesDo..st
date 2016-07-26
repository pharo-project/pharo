accessing
allClassesDo: aBlock
	| seen evalBlock |
	seen := Set new.
	evalBlock := [ :each | 
		seen add: each first name.
		aBlock
			value: each first;
			value: each last ].
	newClasses do: evalBlock.
	changedClasses do: evalBlock.
	environment classesDo: [ :each | 
		each isObsolete ifFalse: [
			| class |
			class := each theNonMetaClass.
			((seen includes: class name) or: [ self hasRemoved: (self classNameFor: class) ]) ifFalse: [
				(class := self classFor: each) isNil ifFalse: [
					seen add: class name.
					aBlock value: class;
						    value: class theMetaClass ] ] ] ]