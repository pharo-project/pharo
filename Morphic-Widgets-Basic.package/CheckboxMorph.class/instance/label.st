accessing
label
	"Answer the contents of the label morph."

	^(self labelMorph ifNil: [^'']) contents