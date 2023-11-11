Extension { #name : 'BlockClosure' }

{ #category : '*Metacello-Core' }
BlockClosure >> execute: projectSpecBlock against: aScriptExecutor [
    aScriptExecutor executeBlock: self do: projectSpecBlock
]

{ #category : '*Metacello-Core' }
BlockClosure >> setAuthorInMetacelloConfig: aMetacelloConfig [

	aMetacelloConfig setAuthorWithBlock: self
]

{ #category : '*Metacello-Core' }
BlockClosure >> setBaseline: aString withInMetacelloConfig: aMetacelloConfig [
    aMetacelloConfig setBaseline: aString withBlock: self
]

{ #category : '*Metacello-Core' }
BlockClosure >> setBlessingInMetacelloConfig: aMetacelloConfig [

	aMetacelloConfig setBlessingWithBlock: self
]

{ #category : '*Metacello-Core' }
BlockClosure >> setConfiguration: aString withInMetacelloConfig: aMetacelloConfig [
    aMetacelloConfig setConfiguration: aString withBlock: self
]

{ #category : '*Metacello-Core' }
BlockClosure >> setDescriptionInMetacelloConfig: aMetacelloConfig [

	aMetacelloConfig setDescriptionWithBlock: self
]

{ #category : '*Metacello-Core' }
BlockClosure >> setPackage: aString withInMetacelloConfig: aMetacelloConfig [

	aMetacelloConfig setPackage: aString withBlock: self
]

{ #category : '*Metacello-Core' }
BlockClosure >> setProject: aString withInMetacelloConfig: aMetacelloConfig [

	aMetacelloConfig setProject: aString withBlock: self
]

{ #category : '*Metacello-Core' }
BlockClosure >> setTimestampInMetacelloConfig: aMetacelloConfig [

	aMetacelloConfig setTimestampWithBlock: self
]

{ #category : '*Metacello-Core' }
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
