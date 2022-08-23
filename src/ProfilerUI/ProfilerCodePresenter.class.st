"
I show the source code of the selected node in the profiler results tree
"
Class {
	#name : #ProfilerCodePresenter,
	#superclass : #SpCodePresenter,
	#category : #'ProfilerUI-View'
}

{ #category : #initialization }
ProfilerCodePresenter >> initialize [ 
	super initialize.
	self beNotEditable
]

{ #category : #initialization }
ProfilerCodePresenter >> methodOrBlock: methodOrBlock [

	methodOrBlock ifNil: [ 
		self text: ''.
		self beForMethod: nil.
		^ self ].

	self text: methodOrBlock sourceCode.
	self beForMethod: methodOrBlock originMethod.
	methodOrBlock isCompiledBlock
		ifTrue: [ 
			self addTextSegmentDecoration:
				(SpTextPresenterDecorator forHighlight
					 interval: (methodOrBlock sourceNode sourceInterval first to:
							  methodOrBlock sourceNode sourceInterval last + 1);
					 yourself) ]
		ifFalse: [ self removeAllTextSegmentDecorations ]
]
