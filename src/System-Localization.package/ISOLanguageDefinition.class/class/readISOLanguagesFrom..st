private
readISOLanguagesFrom: stream
	"self readISOLanguagesFrom: self isoLanguages readStream "
	| languages language code3 index line |
	languages := Dictionary new.
	[stream atEnd
		or: [(line := stream nextLine readStream) atEnd]]
		whileFalse: [
			language := self new.
			code3 := line upTo: Character tab.
			(index := code3 indexOf: $/) > 0
				ifTrue: [
					language iso3: (code3 copyFrom: 1 to: index-1).
					language iso3Alternate: (code3 copyFrom: index+1 to: code3 size)]
				ifFalse: [language iso3: code3].
			language
				iso2: (line upTo: Character tab);
				language: line upToEnd.
			languages at: language iso3 put: language].
	^languages