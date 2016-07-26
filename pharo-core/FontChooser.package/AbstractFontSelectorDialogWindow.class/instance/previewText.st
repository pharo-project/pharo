accessing
previewText
	"Answer the value of previewText"

	^previewText asText addAttribute: (TextEmphasis new emphasisCode: self textEmphasisCode)