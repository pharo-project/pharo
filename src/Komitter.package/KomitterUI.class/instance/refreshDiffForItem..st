private
refreshDiffForItem: item
	diff
		contextClass:
			(item
				ifNotNil: [ 
					item isMethod
						ifTrue: [ item operation targetClass ]
						ifFalse: [ nil ] ]).
	diff
		leftText: (self leftTextFor: item);
		rightText: (self rightTextFor: item).
	^ (item isNil or: [ item added ])
		ifTrue: [ diff showOnlyDestination: true ]
		ifFalse:
			[ 
			(item removed or: [ item isPackage or: [ item isKomitClass and: [ item isDirty not ] ] ])
				ifTrue: [ diff showOnlySource: true ]
				ifFalse: [ diff showBoth ] ]