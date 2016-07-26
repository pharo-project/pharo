accessing
splitFileName
	"Answer my name split on slash boundaries. A directory will have a trailing empty string."
	^ fileName findTokens: '/'.