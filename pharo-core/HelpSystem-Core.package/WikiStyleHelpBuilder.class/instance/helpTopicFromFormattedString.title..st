private conversion
helpTopicFromFormattedString: aString title: aTitle
	| helpString currentLevel section topicStack topic |
	helpString := aString.
	topicStack := Stack new.
	topicStack push: (HelpTopic new title: aTitle).
	currentLevel := 0.
	section := ''.
	helpString 
		linesDo: [:line | 
			(line beginsWith: self sectionStartChar asString)
				ifTrue: [ |  level title |					
					section ~= ''
						ifTrue: [topicStack top contents: section trimLeft.
							section := ''].
					level := 0.
					[(level < line size) and: [(line at: level + 1) = self sectionStartChar]]
						whileTrue: [level := level + 1].
					title := (line copyFrom: level + 1 to: line size) trimLeft.
					level > currentLevel 
						ifTrue: [(level - currentLevel)
							timesRepeat: [topicStack push:  HelpTopic new].
							topicStack top title: title]
						ifFalse: [currentLevel = level
							ifTrue: [topic := topicStack pop.
								topicStack top addSubtopic: topic.
								topicStack push: (HelpTopic new title: title)]
							ifFalse: [
								(currentLevel - level) + 1 
									timesRepeat: [topic := topicStack pop.
										topicStack top addSubtopic: topic].
								topicStack push: (HelpTopic new title: title)]].
					currentLevel := level]
				ifFalse: [section := section, String cr, line]].
	section ~= ''
		ifTrue: [topicStack top contents: section trimLeft].
	[topicStack isEmpty]
		whileFalse: [topic := topicStack pop.
			topicStack isEmpty ifFalse: [topicStack top addSubtopic: topic]].
	^ topic
