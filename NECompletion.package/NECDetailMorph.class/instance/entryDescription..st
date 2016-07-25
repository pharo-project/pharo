accessing
entryDescription: anECEntryDescription 
	| categoryContents entryDescription |
	entryDescription := anECEntryDescription.
	title
		contents: (entryDescription title
				ifNil: [
					description bounds: self titleBounds.
					String new]
				ifNotNil: [
					description bounds: self descriptionBounds.
					entryDescription title]).
	description contentsWrapped: entryDescription description.
	categoryContents := entryDescription label.
	label contents: categoryContents