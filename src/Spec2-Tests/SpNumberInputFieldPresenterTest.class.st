Class {
	#name : #SpNumberInputFieldPresenterTest,
	#superclass : #SpTextInputFieldPresenterTest,
	#category : #'Spec2-Tests-Core-Widgets'
}

{ #category : #running }
SpNumberInputFieldPresenterTest >> classToTest [
	^ SpNumberInputFieldPresenter
]

{ #category : #running }
SpNumberInputFieldPresenterTest >> testWhenClimbRateChangedDo [
	| count result |
	count := 0.
	presenter
		whenClimbRateChangedDo: [ :value | 
			count := count + 1.
			result := value ].
	presenter climbRate: 1.
	self assert: count equals: 1.
	self assert: result equals: 1
]

{ #category : #running }
SpNumberInputFieldPresenterTest >> testWhenDigitsChangedDo [
	| count result |
	count := 0.
	presenter
		whenDigitsChangedDo: [ :value | 
			count := count + 1.
			result := value ].
	presenter digits: 2.
	self assert: count equals: 1.
	self assert: result equals: 2
]

{ #category : #running }
SpNumberInputFieldPresenterTest >> testWhenMaximumChangedDo [
	| count result |
	count := 0.
	presenter
		whenMaximumChangedDo: [ :value | 
			count := count + 1.
			result := value ].
	presenter maximum: 10.
	self assert: count equals: 1.
	self assert: result equals: 10
]

{ #category : #running }
SpNumberInputFieldPresenterTest >> testWhenMinimumChangedDo [
	| count result |
	count := 0.
	presenter
		whenMinimumChangedDo: [ :value | 
			count := count + 1.
			result := value ].
	presenter minimum: 10.
	self assert: count equals: 1.
	self assert: result equals: 10
]

{ #category : #running }
SpNumberInputFieldPresenterTest >> testWhenNumberChangedDo [
	| count result |
	count := 0.
	presenter
		whenNumberChangedDo: [ :value | 
			count := count + 1.
			result := value ].
	presenter number: 10.
	self assert: count equals: 1.
	self assert: result equals: 10
]

{ #category : #running }
SpNumberInputFieldPresenterTest >> testWhenNumberTypeChangedDo [
	| count result |
	count := 0.
	presenter
		whenNumberTypeChangedDo: [ :value | 
			count := count + 1.
			result := value ].
	presenter beFloat.
	self assert: count equals: 1.
	self assert: result equals: Float
]
