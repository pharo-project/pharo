Extension { #name : 'UIManager' }

{ #category : '*Morphic-Deprecated' }
UIManager >> displayProgress: titleString from: minVal to: maxVal during: workBlock [
	"SystemProgressMorph show: titleString from: minVal to:  during: "

	self deprecated: 'This method should not be used. You can either use Jobs directly or use Spec and call your application.'.
	^ workBlock asJob
		  title: titleString;
		  min: minVal;
		  max: maxVal;
		  run
]

{ #category : '*Morphic-Deprecated' }
UIManager >> informUser: aString during: aBlock [
	"Display a message above (or below if insufficient room) the cursor
	 during execution of the given block.
		UIManager default informUser: 'Just a sec!' during: [(Delay forSeconds: 1) wait].
	"
	self deprecated: 'This method should not be used. You can either use Jobs directly or use Spec and call the same method from you rapplication.'.
	^self informUserDuring: [:bar | bar label: aString. aBlock value]
]

{ #category : '*Morphic-Deprecated' }
UIManager >> informUserDuring: aBlock [
	"Display a message as progress	during execution of the given block."

	"UIManager default informUserDuring: [:bar|
		#('one' 'two' 'three') do: [:info|
			bar label: info.
			1 to: 100 do: [:v |
				bar current: v.
				(Delay forMilliseconds: 20) wait]]]"

	self deprecated: 'This method should not be used. You can either use Jobs directly or use Spec and call the same method from you rapplication.'.
	self
		displayProgress: ''
		from: 1
		to: 100
		during: [ :bar | aBlock value: bar ]
]

{ #category : '*Morphic-Deprecated' }
UIManager >> request: aTitleString regex: initialRegexString [
	"Prompt the user for a valid regex.
	 Return nil on cancel or a valid RxMatcher"
	| regex |
	regex := initialRegexString.
	"loop until we get a valid regex string back"
	[
		regex := self
			multiLineRequest: aTitleString
			initialAnswer: regex
			answerHeight: 200.
		"cancelled dialog ==> nil"
		regex ifNil: [ ^ nil ].

		[ ^ regex asRegex ] on: Error do: [ :regexParsingError|
			self defer: [	InformativeNotification signal: 'Bad Regex: ', regexParsingError asString ]].
	] repeat
]

{ #category : '*Morphic-Deprecated' }
UIManager >> requestPassword: queryString [
	"Create an instance of me whose question is queryString. Invoke it centered
	 at the cursor, and answer the string the user accepts. Answer the empty
	 string if the user cancels."

	^self subclassResponsibility
]
