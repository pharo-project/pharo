Extension { #name : 'MorphicUIManager' }

{ #category : '*Morphic-Widgets' }
MorphicUIManager >> abort: aStringOrText [

	^ self widgetFactory abort: aStringOrText
]

{ #category : '*Morphic-Widgets' }
MorphicUIManager >> abort: aStringOrText title: aString [

	^ self widgetFactory abort: aStringOrText title: aString
]

{ #category : '*Morphic-Widgets' }
MorphicUIManager >> alert: aStringOrText [

	^ self widgetFactory alert: aStringOrText
]

{ #category : '*Morphic-Widgets' }
MorphicUIManager >> alert: aStringOrText title: aString [

	^ self widgetFactory alert: aStringOrText title: aString
]

{ #category : '*Morphic-Widgets' }
MorphicUIManager >> alert: aStringOrText title: aString configure: aBlock [

	^ self widgetFactory alert: aStringOrText title: aString configure: aBlock
]

{ #category : '*Morphic-Widgets' }
MorphicUIManager >> chooseDirectory: title [

	^ self widgetFactory chooseDirectory: title
]

{ #category : '*Morphic-Widgets' }
MorphicUIManager >> chooseDirectory: label from: dir [
	"Answer the user choice of a directory."

	| modalMorph realLabel |
	realLabel := label ifNil: ['Choose Directory' translated].
	(ProvideAnswerNotification signal: realLabel) ifNotNil: [:answer |
		^answer ].
	modalMorph := self modalMorph.
	^modalMorph theme
		chooseDirectoryIn: modalMorph
		title: realLabel
		path: (dir ifNotNil: [dir fullName])
]

{ #category : '*Morphic-Widgets' }
MorphicUIManager >> chooseFrom: aList lines: linesArray message: messageString title: aString [
	"Choose an item from the given list. Answer the selected item."

	^(self
		chooseFrom: aList
		values: nil
		lines: linesArray
		message: messageString
		title: aString) ifNil: [0]
]

{ #category : '*Morphic-Widgets' }
MorphicUIManager >> chooseFrom: aList lines: linesArray title: aString [
	"Choose an item from the given list. Answer the index of the selected item."

	^(self
		chooseFrom: aList
		values: nil
		lines: linesArray
		title: aString) ifNil: [0]
]

{ #category : '*Morphic-Widgets' }
MorphicUIManager >> chooseFrom: labelList values: valueList lines: linesArray message: messageString title: aString [
	"Choose an item from the given list. Answer the selected item."

	^ self theme
		chooseIn: self modalMorph
		title: aString
		message: messageString
		labels: labelList
		values: valueList
		lines: linesArray
]

{ #category : '*Morphic-Widgets' }
MorphicUIManager >> chooseFrom: labelList values: valueList lines: linesArray title: aString [
	"Choose an item from the given list. Answer the selected item."

	|modalMorph|
	modalMorph := self modalMorph.
	^modalMorph theme
		chooseIn: modalMorph
		title: aString
		labels: labelList
		values: valueList
		lines: linesArray
]

{ #category : '*Morphic-Widgets' }
MorphicUIManager >> chooseFullFileNameMatching: patterns label: label [
	"Let the user choose a file matching the given patterns"

	|modalMorph|
	modalMorph := self modalMorph.
	^modalMorph theme
		chooseFullFileNameIn: modalMorph
		title: (label ifNil: ['Choose File' translated])
		patterns: patterns
		path: nil
		preview: false
]

{ #category : '*Morphic-Widgets' }
MorphicUIManager >> chooseOrRequestFrom: labelList values: valueList lines: linesArray title: aString [
	"Choose an item from the given list.
	 Answer the value selected of the selected item or the new string entered."

	^ self theme
		chooseOrRequestIn: self modalMorph
		title: aString
		labels: labelList
		values: valueList
		lines: linesArray
]

{ #category : '*Morphic-Widgets' }
MorphicUIManager >> confirm: aStringOrText [
	"Put up a question dialog (without cancel).
	Answer true if the response is yes, false if no.
	This is a modal question--the user must respond yes or no."

	self
		deprecated: 'Use ConfirmationRequest signal: instead.'
		on: '2026-01-19'
		in: #Pharo14.
	^ self confirm: aStringOrText label: 'Question' translated
]

{ #category : '*Morphic-Widgets' }
MorphicUIManager >> confirm: questionStringOrText label: labelStringOrText [
	"Put up a question dialog (without cancel).
	Answer true if the response is yes, false if no.
	This is a modal question--the user must respond yes or no."

	|modalMorph|
	modalMorph := self modalMorph.
	^modalMorph theme
		questionWithoutCancelIn: modalMorph
		text: questionStringOrText
		title: labelStringOrText
]

{ #category : '*Morphic-Widgets' }
MorphicUIManager >> confirm: queryString label: title trueChoice: trueChoice falseChoice: falseChoice cancelChoice: cancelChoice default: defaultOption [
	"Put up a yes/no/cancel menu with caption queryString. The actual wording for the choices will be as provided in the trueChoice, falseChoice and cancelChoice parameters.
	defaultOption should be one of true, false or nil to set the default button.
	Answer true if the response is the true-choice, false if it's the false-choice, nil if the cancelChoice.
	This is a modal question -- the user must respond."

	^ self theme
		customQuestionIn: self modalMorph
		text: queryString
		yesText: trueChoice
		noText: falseChoice
		cancelText: cancelChoice
		default: defaultOption
		title: title
]

{ #category : '*Morphic-Widgets' }
MorphicUIManager >> confirm: aStringOrText orCancel: cancelBlock [
	"Put up a question dialog (with cancel) with the text queryString.
	Answer true if the response is yes, false if no.
	Answer the value of the cancel block if cancelled.
	This is a modal question--the user must respond yes or no or cancel."

	|modalMorph|
	modalMorph := self modalMorph.
	^(modalMorph theme
		questionIn: modalMorph
		text: aStringOrText
		title: 'Question' translated) ifNil: cancelBlock
]

{ #category : '*Morphic-Widgets' }
MorphicUIManager >> confirm: queryString trueChoice: trueChoice falseChoice: falseChoice [
	"Put up a yes/no menu with caption queryString. The actual wording for the two choices will be as provided in the trueChoice and falseChoice parameters. Answer true if the response is the true-choice, false if it's the false-choice.
	This is a modal question -- the user must respond one way or the other."

	^self
		confirm: queryString
		trueChoice: trueChoice
		falseChoice: falseChoice
		cancelChoice: nil
		default: nil
]

{ #category : '*Morphic-Widgets' }
MorphicUIManager >> confirm: queryString trueChoice: trueChoice falseChoice: falseChoice cancelChoice: cancelChoice default: defaultOption [
	"Put up a yes/no/cancel menu with caption queryString. The actual wording for the choices will be as provided in the trueChoice, falseChoice and cancelChoice parameters.
	defaultOption should be one of true, false or nil to set the default button.
	Answer true if the response is the true-choice, false if it's the false-choice, nil if the cancelChoice.
	This is a modal question -- the user must respond."

	^ self
		confirm: queryString
		label: 'Question' translated
		trueChoice: trueChoice
		falseChoice: falseChoice
		cancelChoice: cancelChoice
		default: defaultOption
]

{ #category : '*Morphic-Widgets' }
MorphicUIManager >> edit: aText label: labelString accept: aBlockOrNil [
	"Open an editor on the given string/text"

	| window pane |
	window := (SystemWindow labelled: labelString) model: self.
	pane := RubScrolledTextMorph new
				setText: aText asString;
				yourself.
	aBlockOrNil ifNotNil: [
		pane announcer when: RubTextAcceptRequest do: [:aRequest | aBlockOrNil value: aRequest morph text] for: self].
	window addMorph: pane frame: (0@0 corner: 1@1); openInWorld.
	^ window
]

{ #category : '*Morphic-Widgets' }
MorphicUIManager >> inform: aStringOrText [
	"Display a message for the user to read and then dismiss. The world decides how: a desktop notifies, a plain world opens a dialog"

	self currentWorld notifyUser: aStringOrText
]

{ #category : '*Morphic-Widgets' }
MorphicUIManager >> modalMorph [
	"Answer the morph that should be used to handle modality."
	| sender receiver foundWorld |
	sender := thisContext sender.
	foundWorld := false.
	[foundWorld or: [sender isNil]]
		whileFalse: [receiver := sender receiver.
			((sender selector = #invokeWorldMenu:)
					or: [receiver == self currentWorld
							and: [sender selector == #handleEvent:
									or: [sender selector == #findWindow:]]])
				ifTrue: [foundWorld := true]
				ifFalse: [sender := sender sender]].
	foundWorld
		ifTrue: [^ receiver world
				ifNil: [self currentWorld]].
	^ SystemWindow topWindow
		ifNil: [self currentWorld]
]

{ #category : '*Morphic-Widgets' }
MorphicUIManager >> multiLineRequest: queryString  initialAnswer: defaultAnswer answerHeight: answerHeight [
	"Create a multi-line instance of me whose question is queryString with
	the given initial answer. Answer the string the user accepts.  Answer nil if the user cancels.  An
	empty string returned means that the ussr cleared the editing area and
	then hit 'accept'.  Because multiple lines are invited, we ask that the user
	use the ENTER key, or (in morphic anyway) hit the 'accept' button, to
	submit; that way, the return key can be typed to move to the next line."

	|modalMorph|
	modalMorph := self modalMorph.
	^(modalMorph theme
		textEditorIn: modalMorph
		text: queryString
		title: 'Information Required' translated
		entryText: defaultAnswer
		entryHeight: answerHeight)
			ifNotNil: [:text | text asString]
]

{ #category : '*Morphic-Widgets' }
MorphicUIManager >> newMenuIn: aThemedMorph for: aModel [
	"Answer a new menu."
	"UIManager default"
	^self theme
		newMenuIn: aThemedMorph for: aModel
]

{ #category : '*Morphic-Widgets' }
MorphicUIManager >> proceed: aStringOrText [

	^ self widgetFactory proceed: aStringOrText
]

{ #category : '*Morphic-Widgets' }
MorphicUIManager >> proceed: aStringOrText title: aString [

	^ self widgetFactory proceed: aStringOrText title: aString
]

{ #category : '*Morphic-Widgets' }
MorphicUIManager >> question: aStringOrText [

	^ self widgetFactory question: aStringOrText
]

{ #category : '*Morphic-Widgets' }
MorphicUIManager >> question: aStringOrText title: aString [

	^ self widgetFactory question: aStringOrText title: aString
]

{ #category : '*Morphic-Widgets' }
MorphicUIManager >> questionWithoutCancel: aStringOrText [

	^ self widgetFactory questionWithoutCancel: aStringOrText
]

{ #category : '*Morphic-Widgets' }
MorphicUIManager >> questionWithoutCancel: aStringOrText title: aString [

	^ self widgetFactory questionWithoutCancel: aStringOrText title: aString
]

{ #category : '*Morphic-Widgets' }
MorphicUIManager >> request: queryString initialAnswer: defaultAnswer [
	"Create an instance of me whose question is queryString with the given initial answer. Invoke it centered at the given point, and answer the string the user accepts. Answer the empty string if the user cancels."

	^ self
		  request: queryString
		  initialAnswer: defaultAnswer
		  title: 'Provide the following information'
]

{ #category : '*Morphic-Widgets' }
MorphicUIManager >> request: aStringOrText initialAnswer: defaultAnswer entryCompletion: anEntryCompletion [
	^ self request: aStringOrText initialAnswer: defaultAnswer title: 'Information Required' translated entryCompletion: anEntryCompletion
]

{ #category : '*Morphic-Widgets' }
MorphicUIManager >> request: aStringOrText initialAnswer: defaultAnswer title: aTitle [
	"Create an instance of me whose question is queryString with the given
	initial answer. Answer the string the user accepts.
	The title serves for the window that is opened
	Answer the empty string if the user cancels.
	Allow for interception with a ProvideAnswerNotification handler."

	|modalMorph|
	modalMorph := self modalMorph.
	^(modalMorph theme
		textEntryIn: modalMorph
		text: aStringOrText
		title: aTitle
		entryText: defaultAnswer)
]

{ #category : '*Morphic-Widgets' }
MorphicUIManager >> request: aStringOrText initialAnswer: defaultAnswer title: aTitle entryCompletion: anEntryCompletion [
	"Create an instance of me whose question is queryString with the given
	initial answer. Answer the string the user accepts.
	The title serves for the window that is opened
	Answer the empty string if the user cancels.
	Allow for interception with a ProvideAnswerNotification handler."

	|modalMorph|
	modalMorph := self modalMorph.
	^(modalMorph theme
		textEntryIn: modalMorph
		text: aStringOrText
		title: aTitle
		entryText: defaultAnswer
		entryCompletion: anEntryCompletion)
]

{ #category : '*Morphic-Widgets' }
MorphicUIManager >> showWaitCursorWhile: aBlock [
	Cursor wait showWhile: aBlock
]

{ #category : '*Morphic-Widgets' }
MorphicUIManager >> textEntry: aStringOrText [

	^ self widgetFactory textEntry: aStringOrText
]

{ #category : '*Morphic-Widgets' }
MorphicUIManager >> textEntry: aStringOrText title: aString [

	^ self widgetFactory textEntry: aStringOrText title: aString
]

{ #category : '*Morphic-Widgets' }
MorphicUIManager >> textEntry: aStringOrText title: aString entryText: defaultEntryText [

	^ self widgetFactory textEntry: aStringOrText title: aString entryText: defaultEntryText
]

{ #category : '*Morphic-Widgets' }
MorphicUIManager >> theme [
	^ Smalltalk ui theme
]

{ #category : '*Morphic-Widgets' }
MorphicUIManager >> widgetFactory [

	^ MorphicWidgetFactory new
]
