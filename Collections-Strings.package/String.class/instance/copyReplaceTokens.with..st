copying
copyReplaceTokens: oldSubstring with: newSubstring 
	"Replace all occurrences of oldSubstring that are surrounded
	by non-alphanumeric characters"
	^ self copyReplaceAll: oldSubstring with: newSubstring asTokens: true
	"'File asFile Files File''s File' copyReplaceTokens: 'File' with: 'Snick'"