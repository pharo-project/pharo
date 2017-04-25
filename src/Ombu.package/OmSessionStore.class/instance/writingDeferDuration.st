accessing
writingDeferDuration

	^ writingDeferDuration ifNil: [ OmFileStore defaultWritingDeferDuration ]