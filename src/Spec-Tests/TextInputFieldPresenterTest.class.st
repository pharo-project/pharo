Class {
	#name : #TextInputFieldPresenterTest,
	#superclass : #TextPresenterTest,
	#category : #'Spec-Tests-Core-Widgets'
}

{ #category : #running }
TextInputFieldPresenterTest >> classToTest [
	^ TextInputFieldPresenter
]

{ #category : #tests }
TextInputFieldPresenterTest >> testBeNotPasswordIsSet [

	presenter bePassword: false.
	
	self deny: presenter isPassword
]

{ #category : #tests }
TextInputFieldPresenterTest >> testBePasswordIsSet [

	presenter bePassword.
	
	self assert: presenter isPassword
]

{ #category : #tests }
TextInputFieldPresenterTest >> testBeTextIsSet [
	presenter beText.
	self deny: presenter isPassword
]

{ #category : #tests }
TextInputFieldPresenterTest >> testDefaultIsNotPassword [

	self deny: presenter isPassword
]

{ #category : #tests }
TextInputFieldPresenterTest >> testDefaultMaxLengthIsZero [

	self assert: presenter maxLength equals: 0
]

{ #category : #tests }
TextInputFieldPresenterTest >> testDefaultPlaceholderIsEmpty [

	self assert: presenter placeholder isEmpty
]

{ #category : #tests }
TextInputFieldPresenterTest >> testDefaultTextIsEmpty [

	self assert: presenter text isEmpty
]

{ #category : #tests }
TextInputFieldPresenterTest >> testMaxLengthIsSet [

	presenter maxLength: 10.
	
	self assert: presenter maxLength equals: 10
]

{ #category : #tests }
TextInputFieldPresenterTest >> testMaxLengthTruncatesAlreadyTypedText [

	presenter text: '1234567890 ---'.
	presenter maxLength: 10.
	self assert: presenter text equals: '1234567890'
]

{ #category : #tests }
TextInputFieldPresenterTest >> testMaxLengthTruncatesText [

	presenter maxLength: 10.
	presenter text: '1234567890 ---'.
	self assert: presenter text equals: '1234567890'
]

{ #category : #tests }
TextInputFieldPresenterTest >> testPlaceholderIsSet [

	presenter placeholder: 'enter something...'.
	
	self assert: presenter placeholder equals: 'enter something...'
]

{ #category : #tests }
TextInputFieldPresenterTest >> testTextIsSet [

	presenter text: 'aText'.
	
	self assert: presenter text equals: 'aText'
]

{ #category : #tests }
TextInputFieldPresenterTest >> testWhenPlaceholderChangesRaisesSingleEvent [

	self
		assertWith: [ :times | times = 1 ]
		timesRaisedEvent: #whenPlaceholderChangedDo:
		inPresenter: presenter
		whenDoing: [ presenter placeholder: 'test' ]
]

{ #category : #tests }
TextInputFieldPresenterTest >> testWhenTextChangesRaisesSingleEvent [

	self
		assertWith: [ :times | times = 1 ]
		timesRaisedEvent: #whenTextChangedDo:
		inPresenter: presenter
		whenDoing: [ presenter text: 'test' ]
]
