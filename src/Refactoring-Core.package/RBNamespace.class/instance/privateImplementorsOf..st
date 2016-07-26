private
privateImplementorsOf: aSelector 
	| classes |
	classes := Set new.
	self allClassesDo: [ :class |
		(class directlyDefinesLocalMethod: aSelector)
			ifTrue: [ classes add: class ].
		(class theMetaClass directlyDefinesLocalMethod: aSelector)
			ifTrue: [ classes add: class theMetaClass ] ].
	^ classes