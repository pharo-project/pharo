Class {
	#name : #SpTextInputFieldPresenterTest,
	#superclass : #SpAbstractTextPresenterTest,
	#category : #'Spec2-Tests-Core-Widgets'
}

{ #category : #running }
SpTextInputFieldPresenterTest >> classToTest [
	^ SpTextInputFieldPresenter
]

{ #category : #tests }
SpTextInputFieldPresenterTest >> testBeNotPasswordIsSet [

	presenter bePassword: false.
	
	self deny: presenter isPassword
]

{ #category : #tests }
SpTextInputFieldPresenterTest >> testBePasswordIsSet [

	presenter bePassword.
	
	self assert: presenter isPassword
]

{ #category : #tests }
SpTextInputFieldPresenterTest >> testBeTextIsSet [
	presenter beText.
	self deny: presenter isPassword
]

{ #category : #tests }
SpTextInputFieldPresenterTest >> testDefaultIsNotPassword [

	self deny: presenter isPassword
]

{ #category : #tests }
SpTextInputFieldPresenterTest >> testDefaultMaxLengthIsZero [

	self assert: presenter maxLength equals: 0
]

{ #category : #tests }
SpTextInputFieldPresenterTest >> testDefaultPlaceholderIsEmpty [

	self assert: presenter placeholder isEmpty
]

{ #category : #tests }
SpTextInputFieldPresenterTest >> testDefaultTextIsEmpty [

	self assert: presenter text isEmpty
]

{ #category : #tests }
SpTextInputFieldPresenterTest >> testMaxLengthIsSet [

	presenter maxLength: 10.
	
	self assert: presenter maxLength equals: 10
]

{ #category : #tests }
SpTextInputFieldPresenterTest >> testMaxLengthTruncatesAlreadyTypedText [

	presenter text: '1234567890 ---'.
	presenter maxLength: 10.
	self assert: presenter text equals: '1234567890'
]

{ #category : #tests }
SpTextInputFieldPresenterTest >> testMaxLengthTruncatesText [

	presenter maxLength: 10.
	presenter text: '1234567890 ---'.
	self assert: presenter text equals: '1234567890'
]

{ #category : #tests }
SpTextInputFieldPresenterTest >> testTextIsSet [

	presenter text: 'aText'.
	
	self assert: presenter text equals: 'aText'
]

{ #category : #tests }
SpTextInputFieldPresenterTest >> testWhenPlaceholderChangesRaisesSingleEvent [

	self
		assertWith: [ :times | times = 1 ]
		timesRaisedEvent: #whenPlaceholderChangedDo:
		inPresenter: presenter
		whenDoing: [ presenter placeholder: 'test' ]
]

{ #category : #tests }
SpTextInputFieldPresenterTest >> testWhenTextChangesRaisesSingleEvent [

	self
		assertWith: [ :times | times = 1 ]
		timesRaisedEvent: #whenTextChangedDo:
		inPresenter: presenter
		whenDoing: [ presenter text: 'test' ]
]
