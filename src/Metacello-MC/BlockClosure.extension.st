Extension { #name : #BlockClosure }

{ #category : #'*metacello-mc' }
BlockClosure >> valueSupplyingMetacelloAnswers: aListOfPairs [
	"evaluate the block using a list of questions / answers that might be called upon to
	automatically respond to Object>>confirm: or FillInTheBlank requests"

	^ [ self value ]
		on: ProvideAnswerNotification
		do: [ :notify | 
			| caption |
			caption := notify messageText withSeparatorsCompacted.	"to remove new lines"
			aListOfPairs
				detect: [ :each | 
					caption = each first
						or: [ (caption includesSubstring: each first caseSensitive: false)
								or: [ (each first match: caption)
										or: [ (String includesSelector: #matchesRegex:)
												and: [ [ caption matchesRegex: each first ]
														on: Error
														do: [ :ignored | false ] ] ] ] ] ]
				ifFound: [ :answer | notify resume: answer second ]
				ifNone: [ | outerAnswer |
					outerAnswer := ProvideAnswerNotification
						signal: notify messageText.
					outerAnswer
						ifNil: [ notify resume ]
						ifNotNil: [ notify resume: outerAnswer ] ] ]
]
