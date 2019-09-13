Class {
	#name : #SpImagePresenterTest,
	#superclass : #SpSmokeTest,
	#category : #'Spec2-Tests-Core-Widgets'
}

{ #category : #running }
SpImagePresenterTest >> classToTest [
	^ SpImagePresenter
]

{ #category : #tests }
SpImagePresenterTest >> testAutoScale [
	| count result |
	count := 0.
	presenter
		whenAutoScaleChangeDo: [ :value | 
			result := value.
			count := count + 1 ].
	presenter autoScale: true.
	self assert: count equals: 1.
	self assert: result
]

{ #category : #tests }
SpImagePresenterTest >> testSwitchAutoScale [
	| autoState |
	autoState := presenter autoScale.
	presenter switchAutoscale.
	self assert: presenter autoScale equals: autoState not
]
