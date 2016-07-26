all
roundShadowCornersOf: aMorph on: aCanvas in: bounds displayBlock: displayBlock borderWidth: w corners: aList

	| rounder |
	rounder := CR0.
	w = 1 ifTrue: [rounder := CR1].
	w = 2 ifTrue: [rounder := CR2].
	rounder := rounder copy.
	rounder saveBitsUnderCornersOf: aMorph on: aCanvas in: bounds corners: aList.
	displayBlock value.
	rounder tweakShadowCornersOf: aMorph on: aCanvas in: bounds borderWidth: w corners: aList