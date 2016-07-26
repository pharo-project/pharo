visitor
visitMethodChunk: aChunk

	| theClass theMethod |
	theClass:= self classNamed: aChunk behaviorName.
	theMethod := (RGMethodDefinition
						class: theClass
						selector: aChunk methodSelector)
							protocol: aChunk category;
							sourceCode: aChunk contents;
							stamp: aChunk stamp.
	aChunk isMeta 
		ifTrue: [ 
			theMethod isMetaSide: true.
			theClass theMetaClass addMethod: theMethod. ] 
		ifFalse:[ theClass addMethod: theMethod ]