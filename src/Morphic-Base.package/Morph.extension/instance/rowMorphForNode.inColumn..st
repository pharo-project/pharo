*Morphic-Base
rowMorphForNode: aNode inColumn: aColumn
	| rm |
	rm := Morph new color: Color transparent;
			layoutPolicy: RowLayout new;
			hResizing: #shrinkWrap;
			vResizing: #shrinkWrap;
			layoutInset: aColumn container columnInset @ aColumn container rowInset;
			listDirection: #leftToRight;
			cellPositioning: #leftCenter;
			cellInset: 4@0;
			yourself.
			
	rm addMorph: self.
	
	aColumn isFirstColumn 
		ifTrue: [ | icon |
			icon := aColumn container iconBlock value: aNode.
			icon ifNotNil: [ rm addMorph: icon asMorph ].
			"for first column we don't use horizontal inset"
			rm  layoutInset: 0 @ aColumn container rowInset.
		].

	^ rm
