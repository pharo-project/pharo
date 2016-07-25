instance creation
colorFrom: parm
	"Return an instantiated color from parm.  If parm is already a color, return it, else return the result of my performing it if it's a symbol or, if it is a list, it can either be an array of three numbers, which will be interpreted as RGB values, or a list of symbols, the first of which is sent to me and then the others of which are in turn sent to the prior result, thus allowing entries of the form #(blue darker).  Else just return the thing"

	| aColor firstParm |
	(parm isKindOf: Color) ifTrue: [^ parm].
	(parm isSymbol) ifTrue: [^ self perform: parm].
	(parm isString) ifTrue: [^ self fromString: parm].
	((parm isKindOf: SequenceableCollection) and: [parm size > 0])
		ifTrue:
			[firstParm := parm first.
			(firstParm isKindOf: Number) ifTrue:
				[^ self fromRgbTriplet: parm].
			aColor := self colorFrom: firstParm.
			parm doWithIndex:
				[:sym :ind | ind > 1 ifTrue:
					[aColor := aColor perform: sym]].
			^ aColor].
	^ parm

"
Color colorFrom: #(blue darker)
Color colorFrom: Color blue darker
Color colorFrom: #blue
Color colorFrom: #(0.0 0.0 1.0)
"