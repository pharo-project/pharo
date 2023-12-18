"
I show the source code of the selected node in the profiler results tree
"
Class {
	#name : 'StProfilerCodePresenter',
	#superclass : 'SpCodePresenter',
	#category : 'ProfilerUI-View',
	#package : 'ProfilerUI',
	#tag : 'View'
}

{ #category : 'initialization' }
StProfilerCodePresenter >> initialize [
	super initialize.
	self beNotEditable
]

{ #category : 'initialization' }
StProfilerCodePresenter >> methodOrBlock: methodOrBlock [

	methodOrBlock ifNil: [
		self text: ''.
		self clearInteractionModel.
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
