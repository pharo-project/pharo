"
I can build a Roassal graph to display the matching tree.
"
Class {
	#name : 'ClapMatchGraphBuilder',
	#superclass : 'Object',
	#instVars : [
		'context'
	],
	#category : 'Clap-UI',
	#package : 'Clap-UI'
}

{ #category : 'instance creation' }
ClapMatchGraphBuilder class >> forContext: aClapContext [

	^ self new
		context: aClapContext;
		yourself
]

{ #category : 'adding' }
ClapMatchGraphBuilder >> addChildrenOf: aMatch to: aCollection [ 

	aCollection add: aMatch.
	aMatch hasChild 
		ifTrue: [ 
			aMatch children do: [ :child | self addChildrenOf: child to: aCollection ] ].
		
	^ aCollection 
]

{ #category : 'building' }
ClapMatchGraphBuilder >> build [

   | canvas shapes |
    canvas := RSCanvas new.
    shapes := self graphNodes collect: [ :node |
                      | box |
                      box := RSBox new
                                 size: 30;
                                 draggable;
                                 model: node;
                                 yourself.

                      box @ (RSLabeled new text: [ :n | n printString ]) ].

    canvas addAll: shapes.

    RSLineBuilder arrowedLine
        attachPoint: RSBorderAttachPoint new;
        canvas: canvas;
        shapes: canvas nodes;
        connectFrom: #parent.

    RSTreeLayout new
        verticalGap: 50;
        horizontalGap: 150;
        applyOn: canvas nodes.
    canvas @ RSCanvasController.

    ^ canvas
]

{ #category : 'accessing' }
ClapMatchGraphBuilder >> context: aClapContext [ 
	context := aClapContext
]

{ #category : 'computing' }
ClapMatchGraphBuilder >> graphNodes [

	| children |
	children := self addChildrenOf: context match to: OrderedCollection new.
	^ children
]
