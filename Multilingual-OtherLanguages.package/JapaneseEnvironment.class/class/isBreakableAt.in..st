rendering support
isBreakableAt: index in: text

	| prev |
	index = 1 ifTrue: [^ false].
	prev := text at: index - 1.
	prev leadingChar ~= JapaneseEnvironment leadingChar ifTrue: [^ true].
	^ (('、。，．・：；？！゛゜´｀¨＾―‐／\〜‖｜…‥’”）〕］｝〉》」』】°′″℃' includes: (text at: index)) or: ['‘“（〔［｛〈《「『【°′″℃＠§' includes: prev]) not.
