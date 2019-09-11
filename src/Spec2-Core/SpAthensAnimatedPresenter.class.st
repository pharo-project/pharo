Class {
	#name : #SpAthensAnimatedPresenter,
	#superclass : #SpAthensStaticPresenter,
	#instVars : [
		'timeline',
		'updateBlock'
	],
	#category : #'Spec2-Core-Widgets-Athens'
}

{ #category : #specs }
SpAthensAnimatedPresenter class >> adapterName [
	^ #AthensAnimatedAdapter
]

{ #category : #initialization }
SpAthensAnimatedPresenter >> initialize [

	super initialize.
	updateBlock := [  ].
	self timeline: SpTimeline new.
]

{ #category : #initialization }
SpAthensAnimatedPresenter >> tick [
	timeline tick
]

{ #category : #initialization }
SpAthensAnimatedPresenter >> timeline: aTimeline [
	self flag: #comment.
	" Should check what happens if there is arleady a timeline in use "
	timeline := aTimeline.
	timeline whenTickDo: [ :tick | self updateModelToMoment: tick ]
]

{ #category : #initialization }
SpAthensAnimatedPresenter >> updateBlock [
	^ updateBlock
]

{ #category : #initialization }
SpAthensAnimatedPresenter >> updateBlock: aBlock [
	updateBlock := aBlock
]

{ #category : #initialization }
SpAthensAnimatedPresenter >> updateModelToMoment: aTick [
	updateBlock cull: aTick.
	self adapter ifNotNil: [ self adapter updateModelToMoment: aTick]
]
