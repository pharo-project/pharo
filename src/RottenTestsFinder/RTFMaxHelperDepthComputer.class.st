Class {
	#name : #RTFMaxHelperDepthComputer,
	#superclass : #RTFSelfCallVisitor,
	#category : #'RottenTestsFinder-SelfInterpreter'
}

{ #category : #visiting }
RTFMaxHelperDepthComputer >> visitRTFSelfCallNode: aRTFSelfCallNode [
	aRTFSelfCallNode isCallToAssertPrimitive
		ifTrue: [ ^ 0 ].
	^ (self visitRTFSelfCallRootNode: aRTFSelfCallNode) + 1
]

{ #category : #visiting }
RTFMaxHelperDepthComputer >> visitRTFSelfCallRootNode: aRTFSelfCallRootNode [
	^ ((aRTFSelfCallRootNode subCalls
			collect: [ :subCall | subCall acceptVisitor: self ]) ifEmpty: [ #(0) ]) max
]
