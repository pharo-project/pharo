parsing - private
parseEscapePattern

	inputStream next = patternStream next ifFalse: [ invalidPattern := true ]