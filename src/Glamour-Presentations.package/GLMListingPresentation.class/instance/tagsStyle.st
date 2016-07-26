accessing
tagsStyle

	^tagsStyle ifNil:[ self grayRoundedTags. tagsStyle ]