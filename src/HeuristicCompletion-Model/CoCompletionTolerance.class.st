Class {
	#name : 'CoCompletionTolerance',
	#superclass : 'Object',
	#classInstVars : [
		'minimumTokenSize',
		'rate'
	],
	#category : 'HeuristicCompletion-Model-Core',
	#package : 'HeuristicCompletion-Model',
	#tag : 'Core'
}

{ #category : 'accessing' }
CoCompletionTolerance class >> enabled [

	^ self rate > 0.0
]

{ #category : 'accessing' }
CoCompletionTolerance class >> minimumTokenSize [
	^ minimumTokenSize ifNil: [ minimumTokenSize := 2 ]
]

{ #category : 'accessing' }
CoCompletionTolerance class >> minimumTokenSize: anInteger [
	minimumTokenSize := (anInteger ifNil: [ 4 ]) max: 1
]

{ #category : 'accessing' }
CoCompletionTolerance class >> rate [

	^ rate ifNil: [ rate := 0.0 ]
]

{ #category : 'accessing' }
CoCompletionTolerance class >> rate: aNumber [

	| value |
	value := (aNumber ifNil: [ 0.0 ]) asFloat.
	rate := (value max: 0.0) min: 1.0
]

{ #category : 'accessing' }
CoCompletionTolerance class >> reset [

	<script>
	rate := 0.0.
	minimumTokenSize := 2
]
