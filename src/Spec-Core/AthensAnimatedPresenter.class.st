Class {
	#name : #AthensAnimatedPresenter,
	#superclass : #AthensStaticPresenter,
	#traits : 'TSpecObservable',
	#classTraits : 'TSpecObservable classTrait',
	#instVars : [
		'timeline',
		'updateBlock'
	],
	#category : #'Spec-Core-Widgets-Athens'
}

{ #category : #specs }
AthensAnimatedPresenter class >> adapterName [
	^ #AthensAnimatedAdapter
]

{ #category : #initialization }
AthensAnimatedPresenter >> initialize [

	super initialize.
	updateBlock := [  ].
	self timeline: Timeline new.
]

{ #category : #initialization }
AthensAnimatedPresenter >> tick [
	timeline tick
]

{ #category : #initialization }
AthensAnimatedPresenter >> timeline: aTimeline [
	self flag: #comment.
	" Should check what happens if there is arleady a timeline in use "
	timeline := aTimeline.
	timeline whenTickDo: [ :tick | self updateModelToMoment: tick ]
]

{ #category : #initialization }
AthensAnimatedPresenter >> updateBlock [
	^ updateBlock
]

{ #category : #initialization }
AthensAnimatedPresenter >> updateBlock: aBlock [
	updateBlock := aBlock
]

{ #category : #initialization }
AthensAnimatedPresenter >> updateModelToMoment: aTick [
	updateBlock cull: aTick.
	self adapter ifNotNil: [ self adapter updateModelToMoment: aTick]
]
