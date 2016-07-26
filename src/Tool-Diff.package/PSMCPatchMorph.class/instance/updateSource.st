private
updateSource
	"Update the source difference morph."

	|sel|
	sel := self selectedChange.
	sel isNil
		ifTrue: [self diffMorph
				from: ''
				to: '']
		ifFalse: [self diffMorph
					from: sel diffFromSource
					to: sel diffToSource
					contextClass: (sel isClassPatch ifTrue: [nil] ifFalse: [sel targetClass])]